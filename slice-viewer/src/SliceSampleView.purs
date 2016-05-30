module App.SliceSampleView where

import Prelude hiding (div)
import Data.Array (elemIndex, (..))
import Data.Tuple (Tuple(..))
import Data.StrMap as SM
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable (elem)
import Data.Int (toNumber)
import Pux.Html (Html, div)
import Pux.Html.Attributes (className)
import App.Core (AppData)
import Debug.Trace

import Data.SliceSample as Slice
import DataFrame as DF

import Vis.Vega.Splom as Splom

import Util (mapEnum)

data Action
  = UpdateSamples AppData
  | FocusPointFilter AppData
  | SplomAction Splom.Action

type State = 
  { samples :: AppData
  , dims :: Int
  , splom :: Splom.State
  }

init :: Int -> AppData -> State
init dims df =
  { samples: uniqueSamples df
  , dims: dims
  , splom: Splom.init (fields dims) df
  }

update :: Action -> State -> State
update (UpdateSamples df) state = state
  { samples=uniqueSamples df
  , splom=Splom.init (fields state.dims) df
  }
update (FocusPointFilter fps) state = state
  { splom = Splom.update (Splom.HighlightNeighbors nbrs) $
              Splom.update (Splom.HoverPoint splomPts) state.splom }
  where
  fps' = uniqueSamples fps
  splomPts = Splom.splomData fps'
  nbrs = splomNeighbors state.samples fps'
update (SplomAction a) state = state
  { splom=Splom.update a state.splom }

view :: State -> Html Action
view state =
  div [className "focus-points"]
    [ map SplomAction $ Splom.view state.splom
    ]

fields :: Int -> Array String
fields dims = map (\i -> "x" ++ (show i)) (1..dims)

uniqueSamples :: AppData -> AppData
uniqueSamples df = DF.uniqueBy sEq df
  where
  sEq (Slice.SliceSample s1) (Slice.SliceSample s2) = s1.focusPointId == s2.focusPointId

splomNeighbors :: AppData -> AppData -> Array Splom.NeighborRelation
splomNeighbors samples fps = case DF.run fps of
  --[Slice.SliceSample fp] -> map (\fp' -> {source: Splom.splomDatum fp.focusPointId fp.focusPoint, target: fp'}) $ Splom.splomData $ nbrs fp.neighborIds
  [Slice.SliceSample fp] -> Splom.splomData $ nbrs fp.neighborIds
  otherwise -> []
  where
  nbrs nbrIds = DF.rowFilter (\(Slice.SliceSample s) -> elem s.focusPointId nbrIds) samples

splomNeighbors _       _ = []


