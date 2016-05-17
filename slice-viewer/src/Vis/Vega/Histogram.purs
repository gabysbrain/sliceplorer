module Vis.Vega.Histogram where

import Prelude
import Pux.Html (Html, Attribute)

import Vis.Vega (Data, dataAttr)

foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

vegaHistogram :: forall a. Array (Attribute a) -> Data -> Html a
vegaHistogram attrs d = fromReact (attrs ++ [da]) []
  where
    da = dataAttr d

