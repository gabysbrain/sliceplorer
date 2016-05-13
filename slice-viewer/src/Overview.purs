module App.Overview where

import Prelude hiding (div)
import Data.Array (concatMap, snoc, take, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap as SM
import Data.Int as I
import Pux.Html (Html, div, text, input)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Pux.Html.Events (onChange, FormEvent)
import Vis.Vega (vegaChart, toVegaData, allSlicesSpec, histogramSpec)
import Stats (Histogram)
import Debug.Trace

import Data.Samples (SampleGroup(..), DimSamples(..), dims, metricHistograms)
import Data.Slices (Slice(..), Sample(..))

type State = 
  { samples :: SampleGroup
  , samplesToShow :: Int
  }

data Action 
  = UpdateNumberFilter FormEvent

init :: SampleGroup -> State
init sg =
  { samples: sg
  , samplesToShow: 10
  }
  -- FIXME: put all the filters here and filter everything when state changes

update :: Action -> State -> State
update (UpdateNumberFilter ev) state =
  case I.fromString ev.target.value of
       Nothing -> state
       Just n' -> state {samplesToShow=n'}

view :: State -> Html Action
view ({samples=SampleGroup []}) =
  div [] []
view state@{samples=(SampleGroup sg), samplesToShow=n} =
  div [] 
    [ div [className "controls"] 
        [ input [ type_ "range", value (show n)
                , max (show (length sg)), min "0", step "10"
                , onChange UpdateNumberFilter
                ]
                []
        , input [type_ "text", value (show n), onChange UpdateNumberFilter] []
        ]
    , viewDims state
    ]

viewDims :: State -> Html Action
viewDims {samples=sg@(SampleGroup s), samplesToShow=n} =
  div [] $ zipWith (\d h -> viewDimGroup d h redS) (1..(dims sg)) histos
  where
  redS = take n s
  histos = metricHistograms 11 (SampleGroup redS)

viewDimGroup :: Int -> SM.StrMap Histogram -> Array DimSamples -> Html Action
viewDimGroup dim mhs sg =
  div [className "dim-view"]
    --[ viewAllSlices dim sg
     --, statHisto dim sg
    --]
    ([viewAllSlices dim sg] ++ (viewMetricHistograms mhs))

viewAllSlices :: Int -> Array DimSamples -> Html Action
viewAllSlices dim sg =
  vegaChart [] allSlicesSpec jsonSlices
    where jsonSlices = toVegaData $ convertSampleGroup dim sg

convertSampleGroup dim sg =
  concat $ zipWith convert (1..(length sg)) sg
  where
  convert i (DimSamples x) = convertSlice i dim (fromJust $ x.slices !! (dim-1))
  
convertSlice sliceId dim (Slice s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: sliceId, d: dim, x: fst s', y: snd s'}

viewMetricHistograms :: SM.StrMap Histogram -> Array (Html Action)
viewMetricHistograms hs = 
  --SM.fold (\l k v -> l `snoc` (viewMetricHistogram k v)) [] hs
  SM.foldMap (\k v -> [viewMetricHistogram k v]) hs

viewMetricHistogram :: String -> Histogram -> Html Action
viewMetricHistogram name h =
  vegaChart [] histogramSpec (toVegaData $ spy $ convert h)
  where
  convert histo =
    zipWith (\s c -> {bin_start: s, bin_end: s+histo.width, count: c})
      histo.binStarts histo.counts


