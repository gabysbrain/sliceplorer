module Vis.Vega.Splom where

import Prelude hiding (div)
import Data.Function.Uncurried (runFn2)
import Data.StrMap as SM 
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Array (snoc)
import Data.Nullable as N
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import App.Core (AppData)

import Data.SliceSample as Slice

import Vis.Vega (dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaPoint = SM.StrMap Number
--type NeighborRelation = {source :: VegaPoint, target :: VegaPoint}
type NeighborRelation = VegaPoint

type PointHoverEvent = Array VegaPoint

type State = 
  { focusPoints :: Array VegaPoint
  , fields :: Array String
  , hoverPoints :: Array VegaPoint
  , neighborPoints :: Array NeighborRelation
  }

data Action 
  = HoverPoint PointHoverEvent
  | HighlightNeighbors (Array NeighborRelation)

init :: Array String -> Array VegaPoint -> State
init fs sg = 
  { focusPoints: sg
  , fields: fs
  , hoverPoints: []
  , neighborPoints: []
  }

update :: Action -> State -> State
update (HoverPoint p) state = state {hoverPoints=p}
update (HighlightNeighbors pts) state = state {neighborPoints=pts}

onPointHover :: forall action. (PointHoverEvent -> action) -> Attribute action
onPointHover s = runFn2 handler "onPointHover" saniHandler
  where
  saniHandler e = s $ foldl (\a x -> a `snoc` x) [] (N.toMaybe e)

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = [da, fa, ha, hpa, na]
  where
  da = dataAttr $ toVegaData state.focusPoints
  ha = onPointHover HoverPoint
  fa = attr "fields" $ toVegaData state.fields
  hpa = attr "hoverPoint" $ toVegaData state.hoverPoints
  na = attr "data-neighbors" $ toVegaData state.neighborPoints

