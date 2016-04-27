module App.SampleView where

import Prelude
import Data.Tuple (fst, snd)
import Data.String (joinWith)
import Data.Array (zipWith, (..), length)
import Vis.Vega (vegaChart)

import Data.Samples (DimSamples(..), Samples)

import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className)

type State = DimSamples

data Action = Null

view :: State -> Html Action
view (DimSamples {focusPoint=fp, slices=s}) = div [className "sample"] 
  [ viewFocusPoint fp
  , div [className "sample-slices"] $ zipWith viewSingleSlice (0..(length s)) s
  ]

viewFocusPoint :: Array Number -> Html Action
viewFocusPoint fp =
  div [className "sample-focus-point"]
    [text $ "Point: " ++ joinWith ", " (map show fp)]

viewSingleSlice :: Int -> Samples -> Html Action
viewSingleSlice dim s = vegaChart [className "dim-slice"] ("x" ++ show (dim+1)) jsonSamples
  where
  jsonSamples = map (\x -> {"x": fst x, "y": snd x}) s



