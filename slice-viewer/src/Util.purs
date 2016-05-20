module Util where

import Prelude
import Data.Array (zipWith, length, (..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.StrMap as SM

mapEnum :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapEnum f xs = zipWith f (0..(length xs - 1)) xs

mapCombine :: forall a b. (a -> b -> a) -> SM.StrMap a -> SM.StrMap b -> SM.StrMap a
mapCombine f ma mb =
  foldl handleKey SM.empty $ SM.toList ma
  where
  handleKey mm (Tuple ka va) = case SM.lookup ka mb of
                                    Just vb -> SM.insert ka (f va vb) mm
                                    Nothing -> SM.insert ka va mm

