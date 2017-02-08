
module Data.Convert where

import Prelude
import App.Core (AppData)
import Data.SliceSample as Slice
import Data.Slices (Sample(..), xLoc, yLoc)

import Data.DataFrame as DF
import Util (unsafeJust, mapEnum)
import Data.Array (concatMap, findIndex, last, snoc, (!!))
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe
import Data.Samples (FocusPoint)
import Data.StrMap as SM 
import Data.Tuple (Tuple(..), fst, snd)

predictValue :: Array Sample -> Number -> Number
predictValue slice x = 
  let upper = findIndex (\x' -> x <= xLoc x') slice
   in case upper of
           Just u | u == 0 -> yLoc $ unsafeJust (slice !! 0)
           -- ideally we average the neighboring slice values to compute 
           -- the focus point y-value
           Just u  -> lerp (xLoc $ unsafeJust (slice !! u)) 
                           (xLoc $ unsafeJust (slice !! (u-1)))
                           (yLoc $ unsafeJust (slice !! u)) 
                           (yLoc $ unsafeJust (slice !! (u-1)))
                           x
           Nothing -> yLoc $ unsafeJust (last slice)

lerp :: Number -> Number -> Number -> Number -> Number -> Number
lerp x1 x2 y1 y2 x =
  y1 + (y2 - y1) * pct
  where
  pct = (x - x1) / (x2 - x1)


