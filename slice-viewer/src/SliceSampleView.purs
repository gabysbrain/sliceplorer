module App.SliceSampleView where

import Prelude hiding (div)
import Data.Array (elemIndex, (..))
import Data.Tuple (Tuple(..))
import Data.StrMap as SM
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Int (toNumber)
import Pux.Html (Html, div)
import Pux.Html.Attributes (className)

import Data.Samples (SampleGroup(..), FocusPoint, dims, focusPoint)
import Vis.Vega.Splom as Splom

import Util (mapEnum)

data Action
  = UpdateSamples SampleGroup
  | FocusPointFilter (Maybe FocusPoint)
  | SplomAction Splom.Action

type State = 
  { samples :: SampleGroup
  , splom :: Splom.State
  }

init :: SampleGroup -> State
init sg =
  { samples: sg
  , splom: Splom.init (fields sg) sg
  }

update :: Action -> State -> State
update (UpdateSamples sg) state = state
  { samples=sg
  , splom=Splom.update (Splom.UpdateSamples sg) state.splom
  }
update (FocusPointFilter fp) state = state
  { splom = Splom.update (Splom.HoverPoint hp) state.splom
  }
  where
  fpId fp' (SampleGroup sg) = toNumber $ fromJust $ elemIndex fp' sg
  hp = map (\fp' -> SM.insert "id" (fpId fp' state.samples) (Splom.splomDatum fp')) 
           fp
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

