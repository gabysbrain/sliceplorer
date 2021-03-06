module Vis.D3.Histogram where

import Prelude
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..))
import Data.Array (zipWith)
import Data.Nullable as N
import Stats (Histogram, HistBin)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)

import Data.ValueRange (ValueRange, minVal, maxVal)

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
-- TODO: add data update action
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
    saniHandler e = h $ map (\x -> {start: x.bin_start, end: x.bin_end, count: x.count, percentage: x.percentage}) $ N.toMaybe e

view :: ValueRange -> Int -> State -> Html Action
view binRange maxCount state = fromReact (attrs binRange maxCount state) []

attrs :: ValueRange -> Int -> State -> Array (Attribute Action)
attrs barRange maxCount state = 
  case state.highlightBar of
       Just h -> [bna, bxa, ca, da, fa, ta, attr "data-highlightBar" h]
       Nothing -> [bna, bxa, ca, da, fa, ta]
  where
  bna = attr "data-binMin" $ minVal barRange
  bxa = attr "data-binMax" $ maxVal barRange
  ca = attr "data-maxCount" maxCount
  da = attr "data-histogram" $ convert state.histogram
  fa = onBarHover HoverBar
  ta = attr "data-highlight" state.highlightTicks
  convert histo =
    zipWith (\s p -> {bin_start: s, bin_end: s+histo.width, percentage: p})
      histo.binStarts histo.percentages

