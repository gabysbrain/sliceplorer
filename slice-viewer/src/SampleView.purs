module App.SampleView where

import Prelude
import Data.Tuple (fst, snd)
import Data.String (joinWith)
import Vis.Vega (vegaChart)

import Data.Samples (DimSamples(..), Samples)

import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className)

type State = DimSamples

data Action = Null

view :: State -> Html Action
view (DimSamples {focusPoint=fp, slices=s}) = div [className "sample"] 
  [ viewFocusPoint fp
  , div [className "sample-slices"] $ map viewSingleSlice s
  ]

viewFocusPoint :: Array Number -> Html Action
viewFocusPoint fp =
  div [className "sample-focus-point"]
    [text $ "Point: " ++ joinWith ", " (map show fp)]

viewSingleSlice :: Samples -> Html Action
viewSingleSlice s = vegaChart [className "dim-slice"] jsonSamples
  where
  jsonSamples = map (\x -> {"x": fst x, "y": snd x}) s



