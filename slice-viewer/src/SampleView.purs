module App.SampleView where

import Prelude
import Data.Tuple (fst, snd)
import Data.String (joinWith)
import Data.Array (zipWith, (..), length, concat)
import Vis.Vega (vegaChart, lineSpec, multiLineSpec, toVegaData)

import Data.Samples (DimSamples(..), Samples)

import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className)

type State = DimSamples

data Action = Null

view :: State -> Html Action
view ds@(DimSamples {focusPoint=fp, slices=s}) = div [className "sample"] 
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
viewSingleSlice :: Int -> Samples -> Html Action
viewSingleSlice dim s = vegaChart [className "dim-slice"] (lineSpec dimName) jsonSamples
  where
  jsonSamples = toVegaData $ map (\x -> {"x": fst x, "y": snd x}) s
  dimName = "x" ++ show dim

viewCompoundSlice :: DimSamples -> Html Action
viewCompoundSlice (DimSamples {slices=ds}) = vegaChart [className "full-slice"] multiLineSpec jsonSamples
  where
  jsonSamples = toVegaData $ concat $ zipWith (\d s -> map (\x -> {"d": d, "x": fst x, "y": snd x}) s) 
                                              (1..(length ds)) 
                                              ds

