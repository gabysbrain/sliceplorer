module Vis.Vega.Slices where

import Prelude 
import Data.Function (runFn2)
import Data.Array (concatMap, mapMaybe, snoc, last, findIndex, findLastIndex, (!!))
import Data.Foldable (foldl)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple, fst, snd)
import Data.Nullable as N
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import App.Core (AppData)

import DataFrame as DF
import Data.SliceSample as Slice
import Data.Slices (Sample(..), xLoc, yLoc)
import Data.ValueRange (ValueRange, minVal, maxVal)

import Vis.Vega (dataAttr, toVegaData)

import Debug.Trace

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaSlicePoint = 
  { slice_id :: Int
  , d :: Int
  , x :: Number
  , y :: Number
  , fpX :: Number
  , fpY :: Number
  }
type VegaHoverPoint = VegaSlicePoint

type SliceHoverEvent = Array VegaSlicePoint

data Action
  = HoverSlice (Array VegaSlicePoint)
  | HighlightNeighbors (Array VegaSlicePoint)

type State = 
  {
    slices :: Array VegaSlicePoint
  , hoverSlice :: Array VegaSlicePoint
  , neighbors :: Array VegaSlicePoint
  }

init :: AppData -> State 
init sg = 
  { slices: samples2slices sg
  , hoverSlice: []
  , neighbors: []
  }

update :: Action -> State -> State
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

samples2slices :: AppData -> Array VegaSlicePoint
samples2slices df = concatMap sample2slice $ DF.run df

sample2slice :: Slice.SliceSample -> Array VegaSlicePoint
sample2slice (Slice.SliceSample s) =
  map convertSample s.slice
  where 
  focusPtX = fromJust $ s.focusPoint !! s.d
  convertSample (Sample s') = 
    { slice_id: s.focusPointId
    , d: s.d
    , x: fst s'
    , y: snd s'
    , fpX : focusPtX
    , fpY: predictValue s.slice focusPtX
    }

predictValue :: Array Sample -> Number -> Number
predictValue slice x = 
  let upper = findIndex (\x' -> x <= yLoc x') slice
   in case upper of
           Just u | u == 0 -> yLoc $ fromJust (slice !! 0)
           -- ideally we average the neighboring slice values to compute 
           -- the focus point y-value
           Just u  -> ((yLoc $ fromJust (slice !! u)) +
                       (yLoc $ fromJust (slice !! (u-1)))) /
                      2.0
           Nothing -> yLoc $ fromJust (last slice)

