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

type PointHoverEvent = Array Int
type HoverRec = {id :: Int}

type State = 
  { focusPoints :: Array VegaPoint
  , fields :: Array String
  , hoverPoints :: Array Int -- focus point ids
  }

data Action 
  = UpdateSamples AppData
  | HoverPoint PointHoverEvent

init :: Array String -> AppData -> State
init fs sg = 
  { focusPoints: splomData sg
  , fields: fs
  , hoverPoints: []
  }

update :: Action -> State -> State
update (UpdateSamples sg) state = state {focusPoints=splomData sg}
update (HoverPoint p) state = state {hoverPoints=p}

onPointHover :: forall action. (PointHoverEvent -> action) -> Attribute action
onPointHover s = runFn2 handler "onPointHover" saniHandler
  where
  saniHandler e = s $ foldl (\a x -> a `snoc` x.id) [] (N.toMaybe e)

view :: State -> Html Action
view state = fromReact (attrs state) []

attrs :: State -> Array (Attribute Action)
attrs state = [da, fa, ha, hpa]
  where
  da = dataAttr $ toVegaData state.focusPoints
  ha = onPointHover HoverPoint
  fa = attr "fields" $ toVegaData state.fields
  hpa = attr "hoverPoint" $ toVegaData $ map (\x -> {id: x}) state.hoverPoints

splomData :: AppData -> Array VegaPoint
splomData df = map (\(Slice.SliceSample s) -> splomDatum s.focusPointId s.focusPoint) $ DF.run df

splomDatum :: Int -> FocusPoint -> VegaPoint
splomDatum id fp = SM.fromFoldable $ named `snoc` (Tuple "id" (toNumber id))
  where
  named = mapEnum (\i x -> Tuple ("x"++(show (i+1))) x) $ fp

{--focusPoint :: SampleGroup -> VegaPoint -> FocusPoint--}
{--focusPoint (SampleGroup sg) vp = fromJust $ sg !! fpId--}
  {--where --}
  {--fpId = floor $ fromJust $ SM.lookup "id" vp--}

