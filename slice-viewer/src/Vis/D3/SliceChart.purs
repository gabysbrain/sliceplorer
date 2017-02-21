
module Vis.D3.SliceChart where

import Prelude
import Data.Foldable (foldl)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable as N
import Data.SliceSample (SliceSample)
import Data.ValueRange (ValueRange, minVal, maxVal)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)

type SliceHoverEvent = Array SliceSample
type SliceClickEvent = Array SliceSample
type XAxisHoverEvent = Maybe Number
type XAxisClickEvent = Maybe Number

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

data Action
  = UpdateSlices (Array SliceSample)
  | HighlightSlices (Array SliceSample)
  | HoverSlice SliceHoverEvent
  | ClickSlice SliceClickEvent
  | HoverXAxis XAxisHoverEvent
  | ClickXAxis XAxisClickEvent
  | ShowClusters Boolean

type State = 
  { slices :: Array SliceSample
  , xRange :: ValueRange
  , yRange :: ValueRange
  , highlight :: Array SliceSample
  , showClusters :: Boolean
  }

init :: ValueRange -> ValueRange -> Array SliceSample -> State
init xRange yRange slices =
  { slices: slices
  , xRange: xRange
  , yRange: yRange
  , highlight: []
  , showClusters: false
  }

onSliceClick :: forall action. (SliceClickEvent -> action) -> Attribute action
onSliceClick s = runFn2 handler "onSliceClick" saniHandler
  where
  saniHandler e = s $ fromMaybe [] (N.toMaybe e)

onSliceHover :: forall action. (SliceHoverEvent -> action) -> Attribute action
onSliceHover s = runFn2 handler "onSliceHover" saniHandler
  where
  saniHandler e = s $ fromMaybe [] (N.toMaybe e)

onXAxisClick :: forall action. (XAxisClickEvent -> action) -> Attribute action
onXAxisClick s = runFn2 handler "onXAxisClick" saniHandler
  where
  saniHandler e = s $ N.toMaybe e

onXAxisHover :: forall action. (XAxisHoverEvent -> action) -> Attribute action
onXAxisHover s = runFn2 handler "onXAxisHover" saniHandler
  where
  saniHandler e = s $ N.toMaybe e

update :: Action -> State -> State
update (UpdateSlices s) state = state { slices = s }
update (ShowClusters cs) state = state { showClusters = cs }
update (HighlightSlices hs) state = state { highlight = hs }
-- these following events don't do anything, 
-- just give hooks for other components
update (HoverSlice ev) state = state
update (ClickSlice ev) state = state
update (HoverXAxis ev) state = state
update (ClickXAxis ev) state = state

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = 
  [ attr "data-data" state.slices
  , attr "data-highlight" state.highlight
  , attr "data-showclusters" state.showClusters
  , attr "data-minVal" $ minVal state.yRange
  , attr "data-maxVal" $ maxVal state.yRange
  , attr "data-minX" $ minVal state.xRange
  , attr "data-maxX" $ maxVal state.xRange
  , onSliceHover HoverSlice
  , onSliceClick ClickSlice
  , onXAxisHover HoverXAxis
  , onXAxisClick ClickXAxis
  ]

