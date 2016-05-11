module App.Overview where

import Prelude hiding (div)
import Data.Array (concatMap, take, zip, zipWith, concat, length, (..))
import Data.Tuple (fst, snd)
import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className)
import Vis.Vega (vegaChart, toVegaData, allSlicesSpec)

import Data.Samples (SampleGroup(..), DimSamples(..), dims)
import Data.Slices (Slice(..), Sample(..))

type State = SampleGroup

data Action = Null

view :: State -> Html Action
view (SampleGroup []) =
  div [] []
view sg =
  div [] $ map (flip viewDimGroup sg) (1..(dims sg))
  --div [] $ map (flip viewDimGroup sg) (0..0)

viewDimGroup :: Int -> SampleGroup -> Html Action
viewDimGroup dim sg =
  div [className "dim-view"]
    [ viewAllSlices dim sg
    ]

viewAllSlices :: Int -> SampleGroup -> Html Action
viewAllSlices dim (SampleGroup sg) =
  vegaChart [] allSlicesSpec jsonSlices
  where jsonSlices = toVegaData $ convertSampleGroup dim sg

convertSampleGroup dim sg =
  concat $ zipWith convert (1..(length sg)) sg
  where
  convert i (DimSamples x) = concatMap (convertSlice i dim) x.slices
  
convertSlice sliceId dim (Slice s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: sliceId, d: dim, x: fst s', y: snd s'}

