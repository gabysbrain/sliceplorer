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

type State = 
  { dim :: Int
  , slices :: Array VegaSlicePoint
  , hoverSlice :: Array VegaSlicePoint
  }

init :: Int -> AppData -> State 
init d sg = 
  { dim: d
  , slices: convertSamples sg
  , hoverSlice: []
  }

update :: Action -> State -> State
update (HoverSlice ev) state = state {hoverSlice=ev}

onSliceHover :: forall action. (SliceHoverEvent -> action) -> Attribute action
onSliceHover s = runFn2 handler "onSliceHover" saniHandler
  where
  saniHandler e = s $ foldl (\a x -> a `snoc` x) [] (N.toMaybe e)

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = [da, fa, ha]
  where
  da = dataAttr $ toVegaData state.slices
  fa = onSliceHover HoverSlice
  ha = attr "hoverSlice" $ toVegaData state.hoverSlice

convertSamples :: AppData -> Array VegaSlicePoint
convertSamples df = concatMap convertSlice $ DF.run df
  
convertSlice :: Slice.SliceSample -> Array VegaSlicePoint
convertSlice (Slice.SliceSample s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: s.focusPointId, d: s.d, x: fst s', y: snd s'}

