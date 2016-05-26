module App.DimensionView where

import Prelude hiding (div)
import Data.StrMap as SM
import Data.Maybe (Maybe(..))
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className)
import Stats (Histogram, histogram)
import Util (mapCombine)
import App.Core (AppData)

import Data.Samples (combineMaps)

import DataFrame as DF
import Data.SliceSample as Slice

import Vis.Vega.Histogram as H
import Vis.Vega.Slices as SV

type State =
  { dim :: Int
  , sliceView :: SV.State
  , histogramStates :: SM.StrMap H.State
  }

data Action
  = UpdateSamples AppData
  | FocusPointFilter AppData
  | SliceViewAction SV.Action
  | HistoAction String H.Action

--update (UpdateSamples sg)
init :: Int -> AppData -> State
init d df =
  { dim: d
  , sliceView: SV.init df
  , histogramStates: map H.init histos
  }
  where 
  histos = metricHistograms 11 df

update :: Action -> State -> State
update (UpdateSamples df) state =
  state { sliceView = SV.update (SV.UpdateSamples df) state.sliceView
        , histogramStates = map H.init histos
        }
  where 
  histos = metricHistograms 11 df
update (FocusPointFilter fp) state = state
  { sliceView = SV.update (SV.HoverSlice sliceIds) state.sliceView
  , histogramStates = if SM.isEmpty metricHighlights
                         then map (\s -> H.update (H.ShowTicks []) s) state.histogramStates
                         else mapCombine (\s x -> H.update (H.ShowTicks x) s) 
                                         state.histogramStates 
                                         metricHighlights
  }
  where 
  --fps = DF.run $ DF.rowFilter (\(Slice.SliceSample s) -> s.d==state.dim) fp
  fps = DF.run fp
  sliceIds = map (\(Slice.SliceSample fp) -> fp.focusPointId) fps
  metricHighlights = combineMaps $ map (\(Slice.SliceSample fp) -> fp.metrics) fps
update (SliceViewAction a) state =
  state {sliceView=SV.update a state.sliceView}
update (HistoAction n a) state =
  state {histogramStates=newHisto}
  where 
  newHisto = SM.update (\hs -> Just $ H.update a hs) n state.histogramStates

view :: State -> Html Action
view {dim=dim, histogramStates=mhs, sliceView=svState} =
  div [className "dim-view"]
    [ div [className "dim-name"] [text $ "dim " ++ (show dim)]
    , div [className "dim-charts"] 
        [ viewAllSlices svState
        , viewMetricHistograms mhs
        ]
    ]

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

