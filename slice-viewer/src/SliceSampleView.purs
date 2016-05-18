module App.SliceSampleView where

import Prelude hiding (div)
import Data.Array ((..))
import Data.Tuple (Tuple(..))
import Data.StrMap as SM
import Pux.Html (Html, div)
import Pux.Html.Attributes (className)

import Data.Samples (SampleGroup(..), DimSamples, dims, focusPoint)
import Vis.Vega.Splom as Splom

import Util (mapEnum)

data Action
  = UpdateSamples SampleGroup

type State = 
  { fieldNames :: Array String
  , focusPoints :: Array Splom.VegaPoint
  }

init :: SampleGroup -> State
init sg =
  { fieldNames: fields sg
  , focusPoints: splomData sg
  }

update :: Action -> State -> State
update (UpdateSamples sg) state = state
  { fieldNames = fields sg
  , focusPoints = splomData sg
  }

view :: State -> Html Action
view state =
  div [className "focus-points"]
    [ Splom.view state.fieldNames state.focusPoints
    ]

fields :: SampleGroup -> Array String
fields sg = map (\i -> "x" ++ (show i)) (1..(dims sg))

splomData :: SampleGroup -> Array Splom.VegaPoint
splomData (SampleGroup sg) = map splomDatum sg

splomDatum :: DimSamples -> Splom.VegaPoint
splomDatum ds = SM.fromFoldable named
  where
  named = mapEnum (\i x -> Tuple ("x"++(show (i+1))) x) $ focusPoint ds

