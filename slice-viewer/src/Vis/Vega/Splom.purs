module Vis.Vega.Splom where

import Prelude hiding (div)
import Data.StrMap as SM
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Pux.Html.Events (handler)

import Vis.Vega (Data, dataAttr, toVegaData)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

type VegaPoint = SM.StrMap Number

type State = Array VegaPoint

view :: forall a. Array String -> State -> Html a
view fields state = fromReact (attrs fields state) []

attrs :: forall a. Array String -> State -> Array (Attribute a)
attrs fields state = [fa, da]
  where
  da = dataAttr $ toVegaData $ state
  fa = attr "fields" $ toVegaData fields

