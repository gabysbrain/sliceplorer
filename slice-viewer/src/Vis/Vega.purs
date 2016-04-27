module Vis.Vega where

import Prelude
import Data.Function (Fn2, runFn2)
import Data.Array (snoc)
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)

--foreign import data Data :: *
type XYVal = {x ::Number, y:: Number}
type Data = Array XYVal

--foreign import parse2 :: Fn2 DOMElement Data Unit

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

dataAttr :: forall a. Data -> Attribute a
dataAttr = attr "data"

vegaChart :: forall a. Array (Attribute a) -> String -> Data -> Html a
vegaChart attrs dim d = fromReact (attrs ++ [attr "xAxisName" dim, dataAttr d]) []

