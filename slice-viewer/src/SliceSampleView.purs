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
import App.Core (AppData)

import Data.SliceSample as Slice
import DataFrame as DF

import Vis.Vega.Splom as Splom

import Util (mapEnum)

data Action
  = UpdateSamples AppData
  | FocusPointFilter (Array Int) -- focus point ids
  | SplomAction Splom.Action

type State = 
  { samples :: AppData
  , dims :: Int
  , splom :: Splom.State
  }

init :: Int -> AppData -> State
init dims df =
  { samples: df
  , dims: dims
  , splom: Splom.init (fields dims) df
  }

update :: Action -> State -> State
update (UpdateSamples df) state = state
  { samples=df
  , splom=Splom.init (fields state.dims) df
  }
update (FocusPointFilter fps) state = state
  { splom = Splom.update (Splom.HoverPoint fps) state.splom }
update (SplomAction a) state = state
  { splom=Splom.update a state.splom }

view :: State -> Html Action
view state =
  div [className "focus-points"]
    [ map SplomAction $ Splom.view state.splom
    ]

fields :: Int -> Array String
fields dims = map (\i -> "x" ++ (show i)) (1..dims)

