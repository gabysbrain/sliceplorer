module App.DimensionView where

import Prelude hiding (div)
import Data.StrMap as SM
import Data.Array (concatMap, snoc, take, zip, zipWith, concat, length, head, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Stats (Histogram, HistBin)
import Util (mapEnum, mapCombine)

import Data.Samples (SampleGroup(..), FocusPoint(..), dims, metricHistograms)
import Data.Slices (Slice(..), Sample(..), Metrics(..), metrics)
import Debug.Trace

import Vis.Vega.Histogram as H
import Vis.Vega.Slices as SV

type State =
  { dim :: Int
  , histograms :: SM.StrMap Histogram
  , sliceView :: SV.State
  , histogramStates :: SM.StrMap H.State
  }

data Action
  = UpdateSamples SampleGroup
  | FocusPointFilter (Maybe FocusPoint)
  | SliceViewAction SV.Action
  | HistoAction String H.Action

--update (UpdateSamples sg)
init :: Int -> SampleGroup -> State
init d sg =
  { dim: d
  , histograms: histos
  , sliceView: SV.init d sg
  , histogramStates: map H.init histos
  }
  where 
  histos = metricHistograms 11 d sg

update :: Action -> State -> State
update (UpdateSamples sg) state =
  state { histograms = histos
        , sliceView = SV.update (SV.UpdateSamples sg) state.sliceView
        , histogramStates = map H.init histos
        }
  where 
  histos = metricHistograms 11 state.dim sg
update (FocusPointFilter fp) state = state
  { sliceView = SV.update (SV.HoverSlice $ map (highlightedSlice state) fp) state.sliceView 
  , histogramStates = 
      case fp of
           Just fp' -> mapCombine (\s x -> H.update (H.ShowTicks [x]) s) 
                                  state.histogramStates 
                                  (sliceMetrics state fp')
           Nothing -> map (\s -> H.update (H.ShowTicks []) s) state.histogramStates
  }
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

highlightedSlice :: State -> FocusPoint -> SV.VegaSlicePoint
highlightedSlice state (FocusPoint fp) =
  fromJust $ head $ SV.convertSlice (SV.sliceId state.sliceView slice) state.dim slice
  where
  slice = fromJust $ fp.slices !! state.dim

sliceMetrics :: State -> FocusPoint -> Metrics
sliceMetrics state (FocusPoint fp) =
  metrics slice
  where
  slice = fromJust $ fp.slices !! state.dim

