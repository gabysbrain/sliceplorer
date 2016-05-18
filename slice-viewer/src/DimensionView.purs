module App.DimensionView where

import Prelude hiding (div)
import Data.StrMap as SM
import Data.Array (concatMap, snoc, take, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Vis.Vega (vegaChart, toVegaData, allSlicesSpec)
import Stats (Histogram, HistBin)
import Util (mapEnum)

import Data.Samples (SampleGroup(..), DimSamples(..), dims, metricHistograms)
import Data.Slices (Slice(..), Sample(..))
import Debug.Trace

import Vis.Vega.Histogram as H

type VegaSlice = 
  { slice_id :: Int
  , d :: Int
  , x :: Number
  , y :: Number
  }

type State =
  { dim :: Int
  , histograms :: SM.StrMap Histogram
  , slices :: Array VegaSlice
  , histogramStates :: SM.StrMap H.State
  }

data Action
  = UpdateSamples SampleGroup
  | HistoHighlight String H.Action

--update (UpdateSamples sg)
init :: Int -> SampleGroup -> State
init d sg =
  { dim: d
  , histograms: histos
  , slices: convertSampleGroup d sg
  , histogramStates: map H.init histos
  }
  where 
  histos = metricHistograms 11 d sg

update :: Action -> State -> State
update (UpdateSamples sg) state = state 
  { histograms = histos
  , slices = convertSampleGroup state.dim sg
  , histogramStates = map H.init histos
  }
  where 
  histos = metricHistograms 11 state.dim sg
update (HistoHighlight n a) state =
  state {histogramStates=newHisto}
  where 
  newHisto = SM.update (\hs -> Just $ H.update a hs) n state.histogramStates

view :: State -> Html Action
view {dim=dim, histogramStates=mhs, slices=sg} =
  div [className "dim-view"]
    [ div [className "dim-name"] [text $ "dim " ++ (show dim)]
    , div [className "dim-charts"] 
        [ viewAllSlices dim sg
        , viewMetricHistograms mhs
        ]
    ]

viewAllSlices :: Int -> Array VegaSlice -> Html Action
viewAllSlices dim sg =
  vegaChart [className "slices-view"] allSlicesSpec jsonSlices
    where jsonSlices = toVegaData $ sg

viewMetricHistograms :: SM.StrMap H.State -> Html Action
viewMetricHistograms hs = 
  div [className "metric-histograms"] $
    SM.foldMap (\k v -> [viewMetricHistogram k v]) hs

viewMetricHistogram :: String -> H.State -> Html Action
viewMetricHistogram name h =
  div [className "metric-histogram"]
    [ h3 [className "chart-title"] [text name]
    , map (HistoHighlight name) $ H.view h
    ]

convertSampleGroup dim (SampleGroup sg) =
  concat $ mapEnum convert sg
  where
  convert i (DimSamples x) = convertSlice i dim (fromJust $ x.slices !! dim)
  
convertSlice sliceId dim (Slice s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: sliceId, d: dim, x: fst s', y: snd s'}

