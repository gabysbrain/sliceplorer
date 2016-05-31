module App.GroupView where

import Prelude hiding (div)
import Data.StrMap as SM
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Array (concatMap)
import Data.Foldable (elem, minimum, maximum)
import Data.Tuple (Tuple(..), uncurry)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, key)
import Stats (Histogram, histogram, histogram', histRanges)
import Util (mapCombine, zipMap)
import App.Core (AppData)
import Debug.Trace

import Data.Slices (yLoc)
import Data.Samples (combineMaps)

import DataFrame as DF
import Data.SliceSample as Slice
import Data.ValueRange (ValueRange)

import Vis.Vega.Histogram as H
import Vis.Vega.Slices as SV

type State =
  { key :: Int -- used to force remounting
  , samples :: AppData
  , groupName :: String
  , groupId :: Int
  , sliceViewRange :: ValueRange
  , sliceView :: SV.State
  , histogramRanges :: SM.StrMap Histogram
  , histogramStates :: SM.StrMap H.State
  }

data Action
  = UpdateSamples AppData
  | FocusPointFilter AppData
  | SliceViewAction SV.Action
  | HistoAction String H.Action

init :: AppData -> Int -> String -> Int -> AppData -> State
init origData key gn g df =
  { key: key
  , samples: df
  , groupName: gn
  , groupId: g
  , sliceViewRange: fromJust svRange
  , sliceView: SV.init df
  , histogramRanges: origDataHists
  , histogramStates: map H.init $ metricHistograms' origDataRngs df
  }
  where 
  svRange = DF.range (\(Slice.SliceSample s) -> fromJust $ maximum $ map yLoc s.slice) origData
  origDataHists = metricHistograms 11 origData
  origDataRngs = map histRanges origDataHists

update :: Action -> State -> State
update (UpdateSamples df) state =
  state { samples = df
        , sliceView = SV.init df
        , histogramStates = map H.init $ metricHistograms' origDataRngs df
        }
  where 
  origDataRngs = map histRanges state.histogramRanges
update (FocusPointFilter fp) state = state
  { sliceView = SV.update (SV.HighlightNeighbors neighborSlices) $
                  SV.update (SV.HoverSlice hoverSlices) state.sliceView
  , histogramStates = if SM.isEmpty metricHighlights
                         then map (\s -> H.update (H.ShowTicks []) s) state.histogramStates
                         else mapCombine (\s x -> H.update (H.ShowTicks x) s) 
                                         state.histogramStates 
                                         metricHighlights
  }
  where 
  fps = DF.run fp
  nbrs = findNeighbors state.samples fps
  hoverSlices = concatMap SV.convertSlice fps
  neighborSlices = concatMap SV.convertSlice nbrs
  metricHighlights = combineMaps $ map (\(Slice.SliceSample fp) -> fp.metrics) fps
update (SliceViewAction a) state =
  state {sliceView=SV.update a state.sliceView}
update (HistoAction n a) state =
  state {histogramStates=newHisto}
  where 
  newHisto = SM.update (\hs -> Just $ H.update a hs) n state.histogramStates

findNeighbors :: AppData -> Array Slice.SliceSample -> Array Slice.SliceSample
findNeighbors df [(Slice.SliceSample fp)] =
  DF.run $ DF.rowFilter (neighborFilter fp.neighborIds) df
findNeighbors _  _       = []

neighborFilter :: Array Int -> Slice.SliceSample -> Boolean
neighborFilter fpIds (Slice.SliceSample fp) = elem fp.focusPointId fpIds

view :: State -> Html Action
view state =
  div [className "dim-view", key $ show state.key]
    [ viewName state
    , div [className "dim-charts"] 
        [ viewAllSlices state
        , viewMetricHistograms state
        ]
    ]

viewName :: State -> Html Action
viewName state = div [className "dim-name"] [text groupName]
  where
  groupName = state.groupName ++ " " ++ (show state.groupId)

viewAllSlices :: State -> Html Action
viewAllSlices state =
  div [className "slices-view"] 
    [ map SliceViewAction $ SV.view state.sliceViewRange state.sliceView ]

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
  maxCount = fromJust $ maximum h.counts

metricHistograms :: Int -> AppData -> SM.StrMap Histogram
metricHistograms bins df = map (histogram bins) $ metricData df

metricHistograms' :: SM.StrMap (Array ValueRange) -> AppData -> SM.StrMap Histogram
metricHistograms' binMap df = map (uncurry histogram') $ zipMap binMap (metricData df)

histogramRanges :: AppData -> SM.StrMap ValueRange
histogramRanges df = map rng $ metricData df
  where
  rng xs = Tuple (fromJust $ minimum xs) (fromJust $ maximum xs)

metricData :: AppData -> SM.StrMap (Array Number)
metricData df = combineMaps $ map (\(Slice.SliceSample fp) -> fp.metrics) fps
  where
  fps = DF.run df

