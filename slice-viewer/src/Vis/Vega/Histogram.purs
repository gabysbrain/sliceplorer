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
import Debug.Trace

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Vis.Vega (Data, dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type BarHoverEvent = Maybe HistBin

data Action
  = HoverBar BarHoverEvent

type State = 
  { histogram :: Histogram
  , highlight :: Maybe HistBin
  }

init :: Histogram -> State 
init h = {histogram: h, highlight: Nothing}

update (HoverBar ev) state = state {highlight=ev}

onBarHover :: forall action. (BarHoverEvent -> action) -> Attribute action
onBarHover h = runFn2 handler "onBarHover" saniHandler
  where
  saniHandler e = h $ N.toMaybe e

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = 
  case state.highlight of
       Just h -> [da, attr "highlightBar" h]
       Nothing -> [da]
       {--Just h -> [da, fa, attr "highlightBar" h]--}
       {--Nothing -> [da, fa]--}
  where
  da = dataAttr $ toVegaData $ convert state.histogram
  fa = onBarHover HoverBar
  convert histo =
    zipWith (\s c -> {bin_start: s, bin_end: s+histo.width, count: c})
      histo.binStarts histo.counts

