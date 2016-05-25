module Vis.Vega.Histogram where

import Prelude
import Data.Function (runFn2)
import Data.Maybe (Maybe(..))
import Data.Array (zipWith)
import Data.Nullable as N
import Stats (Histogram, HistBin)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)

import Vis.Vega (dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type BarHoverEvent = Maybe HistBin

data Action
  = HoverBar BarHoverEvent
  | ShowTicks (Array Number)

type State = 
  { histogram :: Histogram
  , highlightBar :: Maybe HistBin
  , highlightTicks :: Array Number
  }

init :: Histogram -> State 
init h = {histogram: h, highlightBar: Nothing, highlightTicks: []}

update :: Action -> State -> State
update (HoverBar ev) state = state 
  { highlightBar = ev
  , highlightTicks = []
  }
update (ShowTicks ts) state = state 
  { highlightBar = Nothing
  , highlightTicks = ts
  }

onBarHover :: forall action. (BarHoverEvent -> action) -> Attribute action
onBarHover h = runFn2 handler "onBarHover" saniHandler
  where
    saniHandler e = h $ map (\x -> {start: x.bin_start, end: x.bin_end, count: x.count}) $ N.toMaybe e

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = 
  case state.highlightBar of
       Just h -> [da, fa, ta, attr "highlightBar" h]
       Nothing -> [da, fa, ta]
  where
  da = dataAttr $ toVegaData $ convert state.histogram
  fa = onBarHover HoverBar
  ta = attr "highlightTicks" $ toVegaData $ state.highlightTicks
  convert histo =
    zipWith (\s c -> {bin_start: s, bin_end: s+histo.width, count: c})
      histo.binStarts histo.counts

