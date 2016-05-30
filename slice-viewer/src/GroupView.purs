module App.GroupView where

import Prelude hiding (div)
import Data.StrMap as SM
import Data.Maybe (Maybe(..))
import Data.Array (concatMap)
import Data.Foldable (elem)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, key)
import Stats (Histogram, histogram)
import Util (mapCombine)
import App.Core (AppData)

import Data.Samples (combineMaps)

import DataFrame as DF
import Data.SliceSample as Slice

import Vis.Vega.Histogram as H
import Vis.Vega.Slices as SV

type State =
  { key :: Int -- used to force remounting
  , samples :: AppData
  , groupName :: String
  , groupId :: Int
  , sliceView :: SV.State
  , histogramStates :: SM.StrMap H.State
  }

data Action
  = UpdateSamples AppData
  | FocusPointFilter AppData
  | SliceViewAction SV.Action
  | HistoAction String H.Action

init :: Int -> String -> Int -> AppData -> State
init key gn g df =
  { key: key
  , samples: df
  , groupName: gn
  , groupId: g
  , sliceView: SV.init df
  , histogramStates: map H.init histos
  }
  where 
  histos = metricHistograms 11 df

update :: Action -> State -> State
update (UpdateSamples df) state =
  state { samples = df
        , sliceView = SV.init df
        , histogramStates = map H.init histos
        }
  where 
  histos = metricHistograms 11 df
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
        [ viewAllSlices state.sliceView
        , viewMetricHistograms state.histogramStates
        ]
    ]

viewName :: State -> Html Action
viewName state = div [className "dim-name"] [text groupName]
  where
  groupName = state.groupName ++ " " ++ (show state.groupId)

viewAllSlices :: SV.State -> Html Action
viewAllSlices svState =
  div [className "slices-view"] 
    [ map SliceViewAction $ SV.view svState ]

viewMetricHistograms :: SM.StrMap H.State -> Html Action
viewMetricHistograms hs = 
  div [className "metric-histograms"] $
    SM.foldMap (\k v -> [viewMetricHistogram k v]) hs

viewMetricHistogram :: String -> H.State -> Html Action
viewMetricHistogram name h =
  div [className "metric-histogram"]
    [ h3 [className "chart-title"] [text name]
    , map (HistoAction name) $ H.view h
    ]

metricHistograms :: Int -> AppData -> SM.StrMap Histogram
metricHistograms bins df =
  map (histogram bins) values
  where 
  fps = DF.run df
  values = combineMaps $ map (\(Slice.SliceSample fp) -> fp.metrics) fps

