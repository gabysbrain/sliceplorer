module Vis.Vega.Slices where

import Prelude
import Data.Function (runFn2)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Array (zipWith, concat, concatMap, snoc, elemIndex, (!!))
import Data.Foldable (foldl, foldMap)
import Data.Tuple (fst, snd)
import Data.Nullable as N
import Stats (Histogram, HistBin)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import Util (mapEnum)
import App.Core (AppData)
import Debug.Trace

import DataFrame as DF
import Data.SliceSample as Slice
import Data.Slices (Sample(..))
import Data.ValueRange (ValueRange, minVal, maxVal)

import Vis.Vega (dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaSlicePoint = 
  { slice_id :: Int
  , d :: Int
  , x :: Number
  , y :: Number
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
  { slices: convertSamples sg
  , hoverSlice: []
  , neighbors: []
  }

update :: Action -> State -> State
update (HoverSlice ev) state = state {hoverSlice=ev}
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

convertSamples :: AppData -> Array VegaSlicePoint
convertSamples df = concatMap convertSlice $ DF.run df
  
convertSlice :: Slice.SliceSample -> Array VegaSlicePoint
convertSlice (Slice.SliceSample s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: s.focusPointId, d: s.d, x: fst s', y: snd s'}

