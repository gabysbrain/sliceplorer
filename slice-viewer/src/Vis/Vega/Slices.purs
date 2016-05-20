module Vis.Vega.Slices where

import Prelude
import Data.Function (runFn2)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Array (zipWith, concat, elemIndex, (!!))
import Data.Tuple (fst, snd)
import Data.Nullable as N
import Stats (Histogram, HistBin)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import Util (mapEnum)
import Debug.Trace

import Data.Samples (SampleGroup(..), FocusPoint(..))
import Data.Slices (Sample(..), Slice(..))

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Vis.Vega (dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaSlicePoint = 
  { slice_id :: Int
  , d :: Int
  , x :: Number
  , y :: Number
  }

type SliceHoverEvent = Maybe VegaSlicePoint

data Action
  = UpdateSamples SampleGroup
  | HoverSlice SliceHoverEvent

type State = 
  { dim :: Int
  , origSlices :: Array Slice
  , slices :: Array VegaSlicePoint
  , hoverSlice :: Maybe VegaSlicePoint
  }

init :: Int -> SampleGroup -> State 
init d sg = 
  { dim: d
  , origSlices: getSlices d sg
  , slices: convertSampleGroup d sg
  , hoverSlice: Nothing
  }

update (UpdateSamples sg) state = 
  state { origSlices = getSlices state.dim sg
        , slices = convertSampleGroup state.dim sg }
update (HoverSlice ev) state = state {hoverSlice=ev}

onSliceHover :: forall action. (SliceHoverEvent -> action) -> Attribute action
onSliceHover s = runFn2 handler "onSliceHover" saniHandler
  where
  saniHandler e = s $ N.toMaybe e

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = 
  case state.hoverSlice of
       Just s -> [da, fa, attr "hoverSlice" s]
       Nothing -> [da, fa]
  where
  da = dataAttr $ toVegaData $ state.slices
  fa = onSliceHover HoverSlice

getSlices :: Int -> SampleGroup -> Array Slice
getSlices dim (SampleGroup sg) =
  map (\(FocusPoint p) -> fromJust $ p.slices !! dim) sg

convertSampleGroup :: Int -> SampleGroup -> Array VegaSlicePoint
convertSampleGroup dim (SampleGroup sg) =
  concat $ mapEnum convert sg
  where
  convert i (FocusPoint x) = convertSlice i dim (fromJust $ x.slices !! dim)
  
convertSlice :: Int -> Int -> Slice -> Array VegaSlicePoint
convertSlice sliceId dim (Slice s) =
  map convertSample s.slice
  where 
  convertSample (Sample s') = {slice_id: sliceId, d: dim, x: fst s', y: snd s'}

sliceId :: State -> Slice -> Int
sliceId state s = fromJust $ elemIndex s state.origSlices

