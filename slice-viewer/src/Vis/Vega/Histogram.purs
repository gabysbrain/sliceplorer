module Vis.Vega.Histogram where

import Prelude
import Data.Function (runFn2)
import Data.Maybe (Maybe(..))
import Data.Array (zipWith)
import Stats (Histogram, HistBin)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Vis.Vega (Data, dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type BarHoverEvent = HistBin

data Action
  = HoverBar BarHoverEvent

type State = 
  { histogram :: Histogram
  , highlight :: Maybe HistBin
  }

init :: Histogram -> State 
init h = {histogram: h, highlight: Nothing}

update (HoverBar ev) state = state {highlight=Just ev}

onBarHover :: forall action. (BarHoverEvent -> action) -> Attribute action
onBarHover = runFn2 handler "onBarHover"

--vegaHistogram :: Array (Attribute Action) -> Data -> Html Action
--vegaHistogram attrs d = fromReact (attrs ++ [da, fa]) []
--vegaHistogram attrs d = fromReact (attrs ++ [da, fa]) []
view :: State -> Html Action
view state = fromReact [da, fa] []
  where
  da = dataAttr $ toVegaData $ convert state.histogram
  fa = onBarHover HoverBar
  convert histo =
    zipWith (\s c -> {bin_start: s, bin_end: s+histo.width, count: c})
      histo.binStarts histo.counts
    
