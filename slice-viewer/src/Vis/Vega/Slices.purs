module Vis.Vega.Slices where

import Prelude 
import Data.Function.Uncurried (runFn2)
import Data.Array (concatMap, mapMaybe, snoc, last, length, findIndex, findLastIndex, (!!))
import Data.Foldable (foldl)
import Data.Maybe
import Data.Tuple (Tuple, fst, snd)
import Data.Nullable as N
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import App.Core (AppData)

import Data.ValueRange (ValueRange, minVal, maxVal)

import Vis.Vega (VegaSlicePoint, VegaHoverPoint, SliceHoverEvent, dataAttr, toVegaData)

import Debug.Trace

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

data Action
  = UpdateSlices (Array VegaSlicePoint)
  | HoverSlice (Array VegaSlicePoint)
  | HighlightNeighbors (Array VegaSlicePoint)

type State = 
  {
    slices :: Array VegaSlicePoint
  , hoverSlice :: Array VegaSlicePoint
  , neighbors :: Array VegaSlicePoint
  }

init :: Array VegaSlicePoint -> State 
init pts = 
  { slices: pts
  , hoverSlice: []
  , neighbors: []
  }

update :: Action -> State -> State
update (UpdateSlices s) state = state
  { slices = s
  }
update (HoverSlice ev) state = state 
  { hoverSlice = ev
  }
update (HighlightNeighbors nbrs) state = state {neighbors=nbrs}

onSliceHover :: forall action. (SliceHoverEvent -> action) -> Attribute action
onSliceHover s = runFn2 handler "onSliceHover" saniHandler
  where
  saniHandler e = s $ foldl (\a x -> a `snoc` x) [] (N.toMaybe e)

view :: ValueRange -> State -> Html Action
view yRange state = fromReact (attrs yRange state) []

attrs :: ValueRange -> State -> Array (Attribute Action)
attrs yRange state = [da, na, fa, ha, mnv, mxv]
  where
  mnv = attr "data-minVal" $ minVal yRange
  mxv = attr "data-maxVal" $ maxVal yRange
  da = dataAttr $ toVegaData state.slices
  na = attr "data-neighbors" $ toVegaData state.neighbors
  fa = onSliceHover HoverSlice
  ha = attr "hoverSlice" $ toVegaData state.hoverSlice

