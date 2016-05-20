module App.DimensionView where

import Prelude hiding (div)
import Data.StrMap as SM
import Data.Array (concatMap, snoc, take, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Stats (Histogram, HistBin)
import Util (mapEnum)

import Data.Samples (SampleGroup(..), FocusPoint(..), dims, metricHistograms)
import Data.Slices (Slice(..), Sample(..))
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

