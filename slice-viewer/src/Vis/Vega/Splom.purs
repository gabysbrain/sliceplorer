module Vis.Vega.Splom where

import Prelude hiding (div)
import Data.Function (runFn2)
import Data.StrMap as SM 
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber, floor)
import Data.Array ((!!))
import Data.Nullable as N
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)
import Debug.Trace
import Util (mapEnum)

import Data.Samples (SampleGroup(..), FocusPoint(..))
import Data.Samples as S

import Vis.Vega (Data, dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaPoint = SM.StrMap Number

type PointHoverEvent = Maybe VegaPoint

type State = 
  { focusPoints :: Array VegaPoint
  , fields :: Array String
  , hoverPoint :: Maybe VegaPoint
  }

data Action 
  = UpdateSamples SampleGroup
  | HoverPoint PointHoverEvent

init :: Array String -> SampleGroup -> State
init fs sg = 
  { focusPoints: splomData sg
  , fields: fs
  , hoverPoint: Nothing
  }

update :: Action -> State -> State
update (UpdateSamples sg) state = state {focusPoints=splomData sg}
update (HoverPoint p) state = state {hoverPoint=p}

onPointHover :: forall action. (PointHoverEvent -> action) -> Attribute action
onPointHover s = runFn2 handler "onPointHover" saniHandler
  where
  saniHandler e = s $ N.toMaybe e

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = 
  case state.hoverPoint of
       Just s -> [da, fa, ha, attr "hoverPoint" s]
       Nothing -> [da, fa, ha]
  where
  da = dataAttr $ toVegaData $ state.focusPoints
  ha = onPointHover HoverPoint
  fa = attr "fields" $ toVegaData state.fields

splomData :: SampleGroup -> Array VegaPoint
splomData (SampleGroup sg) = --map splomDatum sg
  mapEnum (\i x -> SM.insert "id" (toNumber i) $ splomDatum x) sg

splomDatum :: FocusPoint -> VegaPoint
splomDatum ds = SM.fromFoldable named
  where
  named = mapEnum (\i x -> Tuple ("x"++(show (i+1))) x) $ S.focusPoint ds

focusPoint :: SampleGroup -> VegaPoint -> FocusPoint
focusPoint (SampleGroup sg) vp = fromJust $ sg !! fpId
  where 
  fpId = floor $ fromJust $ SM.lookup "id" vp

