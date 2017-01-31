module App.DimView where

import Prelude hiding (div)
import Data.Array (modifyAt, zipWith, concatMap)
import Data.Filterable (filtered)
import Data.Foldable (elem, find, minimum, maximum)
import Data.Int as I
import Data.Maybe
import Data.StrMap as SM
import Data.Tuple (Tuple(..), uncurry)
import Stats (Histogram, histogram, histogram', histRanges)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, key)
--import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum, mapCombine, zipMap, unsafeJust, numRange)

import Data.Slices (yLoc)
import Data.Samples (combineMaps)
import Data.Convert (samples2slices, sample2slice)

import App.Core (AppData, DimData)
import Data.DataFrame (Query)
import Data.DataFrame as DF
import Data.ValueRange (ValueRange)

import Vis.Vega.Histogram as H
import Vis.Vega.Slices as SV
import Vis.Vega.ClusterSlices as CSV
import Data.SliceSample as Slice

import Debug.Trace

type State =
  { dimName :: String
  , samples :: AppData
  , showClusters :: Boolean
  , sliceViewRange :: ValueRange
  , sliceView :: SV.State
  , clusterSliceView :: CSV.State
  , histogramRanges :: SM.StrMap Histogram
  , histogramStates :: SM.StrMap H.State
  }

data Action
  = ShowClusterView Boolean
  | FocusPointFilter AppData
  | UpdateSamples AppData
  | SliceViewAction SV.Action
  | ClusterSliceViewAction CSV.Action
  | HistoAction String H.Action

init :: String -> AppData -> Query AppData State
init dn df = do
  origDataHists <- metricHistograms 11
  yValRange <- functionRange
  let origDataRngs = map histRanges origDataHists
  pure $ { dimName: dn
         , samples: df
         , showClusters: false
         , sliceViewRange: yValRange
         , sliceView: SV.init $ samples2slices df
         , clusterSliceView: CSV.init $ samples2slices df
         , histogramRanges: origDataHists
         , histogramStates: map H.init $ DF.runQuery (metricHistograms' origDataRngs) df
         }

update :: Action -> State -> State
update (ShowClusterView s) state = state {showClusters=s}
update (UpdateSamples df) state =
  state { samples = df
        , sliceView = SV.update (SV.UpdateSlices $ samples2slices df) state.sliceView
        , histogramStates = map H.init $ DF.runQuery (metricHistograms' origDataRngs) df
        }
  where 
  origDataRngs = map histRanges state.histogramRanges
update (FocusPointFilter fp) state = state
  { sliceView        = SV.update (SV.HoverSlice hoverSlices) state.sliceView
  , clusterSliceView = CSV.update (CSV.HoverSlice hoverSlices) state.sliceView
  --{ sliceView = SV.update (SV.HighlightNeighbors neighborSlices) $
                  --SV.update (SV.HoverSlice hoverSlices) state.sliceView
  --, clusterSliceView = CSV.update (CSV.HighlightNeighbors cvNeighborSlices) $
                  --CSV.update (CSV.HoverSlice cvHoverSlices) state.clusterSliceView
  , histogramStates = let mh = DF.runQuery metricHighlights fp
                       in if SM.isEmpty mh
                          then map (\s -> H.update (H.ShowTicks []) s) state.histogramStates
                          else mapCombine (\s x -> H.update (H.ShowTicks x) s) 
                                          state.histogramStates mh
  }
  where 
  --nbrs = findNeighbors state.samples fp
  hoverSlices = samples2slices fp
  --neighborSlices = concatMap sample2slice nbrs
update (SliceViewAction a) state =
  state {sliceView=SV.update a state.sliceView}
update (ClusterSliceViewAction a) state =
  state {clusterSliceView=CSV.update a state.clusterSliceView}
update (HistoAction n a) state =
  state {histogramStates=newHisto}
  where 
  newHisto = SM.update (\hs -> Just $ H.update a hs) n state.histogramStates

metricHighlights :: Query AppData (SM.StrMap (Array Number))
metricHighlights = do
  q <- DF.summarize (\(Slice.SliceSample fp) -> fp.metrics)
  pure $ combineMaps q

view :: State -> Html Action
view state =
  div [className "dim-view"]
    [ viewName state.dimName
    , div [className "dim-charts"]
      [ viewAllSlices state
      , viewMetricHistograms state
      ]
    ]

viewName :: String -> Html Action
viewName name = div [className "dim-name"] [text name]

viewAllSlices :: State -> Html Action
viewAllSlices state =
  div [className "slices-view"] 
    [ sliceView state.showClusters ]
  where 
  sliceView false = 
    map SliceViewAction $ SV.view state.sliceViewRange state.sliceView
  sliceView true = 
    map ClusterSliceViewAction $ CSV.view state.sliceViewRange state.clusterSliceView

viewMetricHistograms :: State -> Html Action
viewMetricHistograms state = 
  div [className "metric-histograms"] $
    SM.foldMap (\k (Tuple r s) -> [viewMetricHistogram k r s]) hs
  where
  hs = zipMap state.histogramRanges state.histogramStates

viewMetricHistogram :: String -> Histogram -> H.State -> Html Action
viewMetricHistogram name h st =
  div [className "metric-histogram"]
    [ h3 [className "chart-title"] [text name]
    , map (HistoAction name) $ H.view rng maxCount st
    ]
  where
  rng = Tuple h.min h.max
  maxCount = unsafeJust $ maximum h.counts

metricHistograms :: Int -> Query AppData (SM.StrMap Histogram)
metricHistograms bins = (map (histogram bins)) <$> metricData

metricHistograms' :: SM.StrMap (Array ValueRange) -> Query AppData (SM.StrMap Histogram)
metricHistograms' binMap = do
  md <- metricData
  pure $ map (uncurry histogram') $ zipMap binMap md

histogramRanges :: Query AppData (SM.StrMap (Maybe ValueRange))
histogramRanges = (map numRange) <$> metricData


metricData :: Query AppData (SM.StrMap (Array Number))
metricData = do
  metrics <- DF.summarize (\(Slice.SliceSample fp) -> fp.metrics)
  pure $ combineMaps metrics

--findNeighbors :: AppData -> Array Slice.SliceSample -> Array Slice.SliceSample
--findNeighbors df [(Slice.SliceSample fp)] =
  --DF.rowFilter (neighborFilter fp.neighborIds) df
--findNeighbors _  _       = []

--neighborFilter :: Array Int -> Slice.SliceSample -> Boolean
--neighborFilter fpIds (Slice.SliceSample fp) = elem fp.focusPointId fpIds

functionRange :: Query AppData (Tuple Number Number)
functionRange = do
  yVals <- DF.summarize maxSliceYLoc
  pure $ fromMaybe (Tuple 0.0 0.0) $ numRange (filtered yVals)

maxSliceYLoc :: Slice.SliceSample -> Maybe Number
maxSliceYLoc (Slice.SliceSample s) = maximum $ map yLoc s.slice

