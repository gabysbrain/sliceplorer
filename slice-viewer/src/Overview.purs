module App.Overview where

import Prelude hiding (div)
import Data.Array (concatMap, take, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Int as I
import Pux.Html (Html, div, text, input)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Pux.Html.Events (onChange, FormEvent)
import Vis.Vega (vegaChart, toVegaData, allSlicesSpec)

import Data.Samples (SampleGroup(..), DimSamples(..), dims)
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

update :: Action -> State -> State
update (UpdateNumberFilter ev) state =
  case I.fromString ev.target.value of
       Nothing -> state
       Just n' -> state {samplesToShow=n'}

view :: State -> Html Action
view ({samples=SampleGroup []}) =
  div [] []
view state@{samples=(SampleGroup sg), samplesToShow=n} =
  --div [] $ map (flip viewDimGroup sg) (0..0)
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
viewDims state@{samples=sg} =
  div [] $ map (flip viewDimGroup state) (1..(dims sg))

viewDimGroup :: Int -> State -> Html Action
viewDimGroup dim state =
  div [className "dim-view"]
    [ viewAllSlices dim state
    --, statHisto dim sg
    ]

viewAllSlices :: Int -> State -> Html Action
viewAllSlices dim {samples=SampleGroup sg, samplesToShow=n} =
  vegaChart [] allSlicesSpec jsonSlices
    where jsonSlices = toVegaData $ convertSampleGroup dim (take n sg)

convertSampleGroup dim sg =
  concat $ zipWith convert (1..(length sg)) sg
  where
  convert i (DimSamples x) = convertSlice i dim (fromJust $ x.slices !! (dim-1))
  
convertSlice sliceId dim (Slice s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: sliceId, d: dim, x: fst s', y: snd s'}


