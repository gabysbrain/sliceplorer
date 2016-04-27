module App.SampleView where

import Prelude
import Data.Tuple (fst, snd)
import Vis.Vega (vegaChart)

import Data.Samples (DimSamples, Samples)

import Pux.Html (Html, div)
import Pux.Html.Attributes (className)

type State = DimSamples

data Action = Null

view :: State -> Html Action
view s = div [className "sample"] $ map viewSingleSlice s

viewSingleSlice :: Samples -> Html Action
viewSingleSlice s = vegaChart [className "dim-slice"] jsonSamples
  where
  jsonSamples = map (\x -> {"x": fst x, "y": snd x}) s



