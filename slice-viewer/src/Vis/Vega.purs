module Vis.Vega where

import Prelude
import Data.Function (Fn2, runFn2)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Data :: *
--type XYVal = {x ::Number, y:: Number}
--type Data = Array *

--foreign import parse2 :: Fn2 DOMElement Data Unit

foreign import data VegaSpec :: *
foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

foreign import histogramSpec :: VegaSpec
foreign import allSlicesSpec :: VegaSpec
foreign import lineSpec :: String -> VegaSpec
foreign import multiLineSpec :: VegaSpec

toVegaData :: forall a. Array a -> Data
toVegaData = unsafeCoerce

dataAttr :: forall a. Data -> Attribute a
dataAttr = attr "data"

specAttr :: forall a. VegaSpec -> Attribute a
specAttr = attr "spec"

vegaChart :: forall a. Array (Attribute a) -> VegaSpec -> Data -> Html a
vegaChart attrs s d = fromReact (attrs ++ [sa, da]) []
  where
    sa = specAttr s
    da = dataAttr d

