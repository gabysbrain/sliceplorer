module App.SampleView where

import Prelude
import Data.Tuple (fst, snd)
import Data.String (joinWith)
import Data.Array (zipWith, (..), length, concat)
import Vis.Vega (vegaChart, lineSpec, multiLineSpec, toVegaData)

import Data.Samples (FocusPoint(..))
import Data.Slices (Slice(..), Sample(..))

import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className)

type State = FocusPoint

data Action = Null

view :: State -> Html Action
view ds@(FocusPoint {focusPoint=fp, slices=s}) = div [className "sample"] 
  [ viewFocusPoint fp
  , div [className "sample-slices"] $ [viewCompoundSlice ds] ++ singleSlices
  ]
  where 
  singleSlices = zipWith viewSingleSlice (1..(length s)) s

viewFocusPoint :: Array Number -> Html Action
viewFocusPoint fp =
  div [className "sample-focus-point"]
    [text $ "Point: " ++ joinWith ", " (map show fp)]

--("x" ++ show (dim+1)) jsonSamples
viewSingleSlice :: Int -> Slice -> Html Action
viewSingleSlice dim s = vegaChart [className "dim-slice"] (lineSpec dimName) jsonSamples
  where
  jsonSamples = toVegaData $ convertSlice dim s
  dimName = "x" ++ show dim

viewCompoundSlice :: FocusPoint -> Html Action
viewCompoundSlice (FocusPoint {slices=ds}) = vegaChart [className "full-slice"] multiLineSpec jsonSamples
  where
  jsonSamples = toVegaData $ concat $ zipWith convertSlice
                                              (1..(length ds)) 
                                              ds

convertSlice d (Slice {slice=s}) = 
  map (\(Sample x) -> {"d": d, "x": fst x, "y": snd x}) s

