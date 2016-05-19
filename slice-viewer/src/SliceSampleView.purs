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
  | SplomAction Splom.Action

type State = 
  { splom :: Splom.State
  }

init :: SampleGroup -> State
init sg =
  { splom: Splom.init (fields sg) sg
  }

update :: Action -> State -> State
update (UpdateSamples sg) state = state
  { splom=Splom.update (Splom.UpdateSamples sg) state.splom
  }
update (SplomAction a) state = state
  {splom=Splom.update a state.splom
  }

view :: State -> Html Action
view state =
  div [className "focus-points"]
    [ map SplomAction $ Splom.view state.splom
    ]

fields :: SampleGroup -> Array String
fields sg = map (\i -> "x" ++ (show i)) (1..(dims sg))

