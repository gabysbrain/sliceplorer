module Util where

import Prelude
import Data.Array (zipWith, length, (..))

mapEnum :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapEnum f xs = zipWith f (0..(length xs - 1)) xs

