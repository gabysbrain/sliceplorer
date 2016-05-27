module Vis.Vega.Splom where

import Prelude hiding (div)
import Data.Function (runFn2)
import Data.StrMap as SM 
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Int (toNumber, floor)
import Data.Array ((!!), snoc)
import Data.Nullable as N
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import Debug.Trace
import Util (mapEnum)
import App.Core (AppData)

import DataFrame as DF
import Data.SliceSample as Slice
import Data.Samples (FocusPoint(..))

import Vis.Vega (Data, dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaPoint = SM.StrMap Number

type PointHoverEvent = Array VegaPoint

type State = 
  { focusPoints :: Array VegaPoint
  , fields :: Array String
  , hoverPoints :: Array VegaPoint
  , neighborPoints :: Array VegaPoint
  }

data Action 
  = HoverPoint PointHoverEvent
  | HighlightNeighbors (Array VegaPoint)

init :: Array String -> AppData -> State
init fs sg = 
  { focusPoints: splomData sg
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

splomData :: AppData -> Array VegaPoint
splomData df = map (\(Slice.SliceSample s) -> splomDatum s.focusPointId s.focusPoint) $ DF.run df

splomDatum :: Int -> FocusPoint -> VegaPoint
splomDatum id fp = SM.fromFoldable $ named `snoc` (Tuple "id" (toNumber id))
  where
  named = mapEnum (\i x -> Tuple ("x"++(show (i+1))) x) $ fp

