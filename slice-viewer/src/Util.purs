module Util where

import Prelude
import Data.Array (zipWith, length, snoc, (..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.StrMap as SM
import Partial.Unsafe (unsafePartial)

mapEnum :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapEnum f xs = zipWith f (0..(length xs - 1)) xs

mapCombine :: forall a b. (a -> b -> a) -> SM.StrMap a -> SM.StrMap b -> SM.StrMap a
mapCombine f ma mb =
  foldl handleKey SM.empty $ SM.toList ma
  where
  handleKey mm (Tuple ka va) = case SM.lookup ka mb of
                                    Just vb -> SM.insert ka (f va vb) mm
                                    Nothing -> SM.insert ka va mm

zipMap :: forall a b. SM.StrMap a -> SM.StrMap b -> SM.StrMap (Tuple a b)
zipMap m1 m2 =
  SM.fold handleFold SM.empty m1
  where
  handleFold mm k v1 = case SM.lookup k m2 of
                            Just v2 -> SM.insert k (Tuple v1 v2) mm
                            Nothing -> mm

numRange :: forall a. Ord a => Array a -> Maybe (Tuple a a)
numRange = foldl range' Nothing
  where
  range' Nothing              x = Just (Tuple x x)
  range' (Just (Tuple mn mx)) x = Just $
    Tuple (if x < mn then x else mn) (if x > mx then x else mx)

unsafeJust :: forall a. Maybe a -> a
unsafeJust x = unsafePartial (fromJust x)

