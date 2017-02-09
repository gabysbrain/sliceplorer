
module Vis.D3.SliceChart where

import Prelude
import Data.Foldable (foldl)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (fromMaybe)
import Data.Nullable as N
import Data.SliceSample (SliceSample)
import Data.ValueRange (ValueRange, minVal, maxVal)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)

import Debug.Trace

type SliceHoverEvent = Array SliceSample

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

data Action
  = UpdateSlices (Array SliceSample)
  | HoverSlice SliceHoverEvent
  | ShowClusters Boolean

type State = 
  { slices :: Array SliceSample
  , yRange :: ValueRange
  , highlight :: Array SliceSample
  , showClusters :: Boolean
  }

init :: ValueRange -> Array SliceSample -> State
init yRange slices =
  { slices: slices
  , yRange: yRange
  , highlight: []
  , showClusters: false
  }

onSliceHover :: forall action. (SliceHoverEvent -> action) -> Attribute action
onSliceHover s = runFn2 handler "onSliceHover" saniHandler
  where
  saniHandler e = s $ fromMaybe [] (N.toMaybe e)

update :: Action -> State -> State
update (UpdateSlices s) state = state { slices = s }
update (ShowClusters cs) state = state { showClusters = cs }
update (HoverSlice ev) state = state { highlight = ev }

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = 
  [ attr "data-data" state.slices
  , attr "data-highlight" state.highlight
  , attr "data-showclusters" state.showClusters
  , attr "data-minVal" $ minVal state.yRange
  , attr "data-maxVal" $ maxVal state.yRange
  , onSliceHover HoverSlice
  ]
