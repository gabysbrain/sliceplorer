
module Data.Convert where

import Prelude
import App.Core (AppData)
import Data.SliceSample as Slice
import Data.Slices (Sample(..), xLoc, yLoc)
import Vis.Vega (VegaSlicePoint, initSlicePoint)
import Vis.Vega.Splom (VegaPoint(..))

import DataFrame as DF
import Util (unsafeJust, mapEnum)
import Data.Array (concatMap, findIndex, last, snoc, (!!))
import Data.Int (toNumber)
import Data.Maybe
import Data.Samples (FocusPoint)
import Data.StrMap as SM 
import Data.Tuple (Tuple(..), fst, snd)

samples2slices :: AppData -> Array VegaSlicePoint
samples2slices df = concatMap sample2slice $ DF.run df

sample2slice :: Slice.SliceSample -> Array VegaSlicePoint
sample2slice (Slice.SliceSample s) =
  map convertSample s.slice
  where 
  focusPtX = unsafeJust $ s.focusPoint !! s.d
  convertSample (Sample s') = 
    { slice_id: s.focusPointId
    , cluster_id: s.clusterId
    , d: s.d
    , x: fst s'
    , y: snd s'
    , fpX : focusPtX
    , fpY: predictValue s.slice focusPtX
    }

splomData :: AppData -> Array VegaPoint
splomData df = map (\(Slice.SliceSample s) -> splomDatum s.focusPointId s.focusPoint) $ DF.run df

splomDatum :: Int -> FocusPoint -> VegaPoint
splomDatum id fp = SM.fromFoldable $ named `snoc` (Tuple "id" (toNumber id))
  where
  named = mapEnum (\i x -> Tuple ("x"<>(show (i+1))) x) $ fp

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


