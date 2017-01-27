module App.DimView where

import Prelude hiding (div)
import Data.Array (modifyAt, zipWith, concatMap)
import Data.Foldable (elem, find, minimum, maximum)
import Data.Int as I
import Data.Maybe
import Data.StrMap as SM
import Data.Tuple (Tuple(..), uncurry)
import Stats (Histogram, histogram, histogram', histRanges)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, key)
--import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum, mapCombine, zipMap, unsafeJust)

import Data.Slices (yLoc)
import Data.Samples (combineMaps)
import Data.Convert (samples2slices, sample2slice)

import App.Core (AppData, DimData, GroupData)
import DataFrame as DF
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

init :: AppData -> String -> AppData -> State
init origDf dn df = 
  { dimName: dn
  , samples: df
  , showClusters: false
  , sliceViewRange: unsafeJust svRange
  , sliceView: SV.init $ samples2slices df
  , clusterSliceView: CSV.init $ samples2slices df
  , histogramRanges: origDataHists
  , histogramStates: map H.init $ metricHistograms' origDataRngs df
  }
  where 
  svRange = DF.range (\(Slice.SliceSample s) -> unsafeJust $ maximum $ map yLoc s.slice) origDf
  origDataHists = metricHistograms 11 origDf
  origDataRngs = map histRanges origDataHists

update :: Action -> State -> State
update (ShowClusterView s) state = state {showClusters=s}
update (UpdateSamples df) state =
  state { samples = df
        , sliceView = SV.update (SV.UpdateSlices $ samples2slices df) state.sliceView
        , histogramStates = map H.init $ metricHistograms' origDataRngs df
        }
  where 
  origDataRngs = map histRanges state.histogramRanges
update (FocusPointFilter fp) state = state
  { sliceView = SV.update (SV.HighlightNeighbors neighborSlices) $
                  SV.update (SV.HoverSlice hoverSlices) state.sliceView
  , clusterSliceView = CSV.update (CSV.HighlightNeighbors cvNeighborSlices) $
                  CSV.update (CSV.HoverSlice cvHoverSlices) state.clusterSliceView
  , histogramStates = if SM.isEmpty metricHighlights
                         then map (\s -> H.update (H.ShowTicks []) s) state.histogramStates
                         else mapCombine (\s x -> H.update (H.ShowTicks x) s) 
                                         state.histogramStates 
                                         metricHighlights
  }
  where 
  fps = DF.run fp
  nbrs = findNeighbors state.samples fps
  hoverSlices = concatMap sample2slice fps
  neighborSlices = concatMap sample2slice nbrs
  cvHoverSlices = concatMap sample2slice fps
  cvNeighborSlices = concatMap sample2slice nbrs
  metricHighlights = combineMaps $ map (\(Slice.SliceSample fp) -> fp.metrics) fps
update (SliceViewAction a) state =
  state {sliceView=SV.update a state.sliceView}
update (ClusterSliceViewAction a) state =
  state {clusterSliceView=CSV.update a state.clusterSliceView}
update (HistoAction n a) state =
  state {histogramStates=newHisto}
  where 
  newHisto = SM.update (\hs -> Just $ H.update a hs) n state.histogramStates

clusterGroups :: AppData -> GroupData
clusterGroups = DF.groupBy groupByCluster
  where
  groupByCluster (Slice.SliceSample s) = I.toNumber s.clusterId

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

metricHistograms :: Int -> AppData -> SM.StrMap Histogram
metricHistograms bins df = map (histogram bins) $ metricData df

metricHistograms' :: SM.StrMap (Array ValueRange) -> AppData -> SM.StrMap Histogram
metricHistograms' binMap df = map (uncurry histogram') $ zipMap binMap (metricData df)

histogramRanges :: AppData -> SM.StrMap ValueRange
histogramRanges df = map rng $ metricData df
  where
  rng xs = Tuple (unsafeJust $ minimum xs) (unsafeJust $ maximum xs)

metricData :: AppData -> SM.StrMap (Array Number)
metricData df = combineMaps $ map (\(Slice.SliceSample fp) -> fp.metrics) fps
  where
  fps = DF.run df

findNeighbors :: AppData -> Array Slice.SliceSample -> Array Slice.SliceSample
findNeighbors df [(Slice.SliceSample fp)] =
  DF.run $ DF.rowFilter (neighborFilter fp.neighborIds) df
findNeighbors _  _       = []

neighborFilter :: Array Int -> Slice.SliceSample -> Boolean
neighborFilter fpIds (Slice.SliceSample fp) = elem fp.focusPointId fpIds


