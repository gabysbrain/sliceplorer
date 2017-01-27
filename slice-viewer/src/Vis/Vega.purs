module Vis.Vega where

import Prelude
import Pux.Html (Html, Attribute)
import Pux.Html.Attributes (attr)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Data :: *
foreign import data VegaSpec :: *
foreign import fromReact :: forall a. Array (Attribute a) -> Array (Html a) -> Html a

foreign import lineSpec :: String -> VegaSpec
foreign import multiLineSpec :: VegaSpec

{--type HoverEvent a =--}
  {--{ datum :: a }--}

toVegaData :: forall a. Array a -> Data
toVegaData = unsafeCoerce

{--onHover :: forall a action. (HoverEvent a -> action) -> Attribute action--}
{--onHover = runFn2 handler "onHover"--}

dataAttr :: forall a. Data -> Attribute a
dataAttr = attr "data"

specAttr :: forall a. VegaSpec -> Attribute a
specAttr = attr "spec"

vegaChart :: forall a. Array (Attribute a) -> VegaSpec -> Data -> Html a
vegaChart attrs s d = fromReact (attrs <> [sa, da]) []
  where
    sa = specAttr s
    da = dataAttr d

type VegaSlicePoint = 
  { slice_id :: Int
  , cluster_id :: Int
  , d :: Int
  , x :: Number
  , y :: Number
  , fpX :: Number
  , fpY :: Number
  }
type VegaHoverPoint = VegaSlicePoint

type SliceHoverEvent = Array VegaSlicePoint

initSlicePoint :: Int -> Int -> Int 
               -> Number -> Number 
               -> Number -> Number 
               -> VegaSlicePoint
initSlicePoint sid cid d x y fpx fpy =
  { slice_id: sid
  , cluster_id: cid
  , d: d
  , x: x
  , y: y
  , fpX: fpx
  , fpY: fpy
  }


