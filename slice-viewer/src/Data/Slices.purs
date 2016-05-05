module Data.Slices where

import Prelude
import Data.Tuple (Tuple(..))
import Data.StrMap (StrMap)
import Data.StrMap as SM
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Data.Foreign.Keys (keys)

type Inputs = Array Number
type Output = Number
type Row = Tuple Inputs Output
newtype Sample = Sample (Tuple Number Number)
data Slice = Slice
  { metrics :: Metrics
  , slice :: Array Sample
  }
type Metrics = StrMap Number

instance sliceIsForeign :: IsForeign Slice where
  read json = do
    v <- readProp "variance" json
    mv <- readProp "min_value" json
    xv <- readProp "max_value" json
    av <- readProp "avg_value" json
    ag <- readProp "avg_gradient" json
    aag <- readProp "avg_pos_gradient" json
    s <- readProp "slice" json
    let m = [ Tuple "variance" v
            , Tuple "min_value" mv
            , Tuple "max_value" xv
            , Tuple "avg_value" av
            , Tuple "avg_gradient" ag
            , Tuple "avg_pos_gradient" aag
            ]
    pure $ Slice {metrics: SM.fromFoldable m, slice: s}

instance sampleIsForeign :: IsForeign Sample where
  read json = do
    x <- readProp "x" json
    y <- readProp "y" json
    pure $ Sample $ Tuple x y

sortBy :: (Slice -> Slice -> Ordering) -> Array Slice -> Array Slice
sortBy cmp = sortBy cmp

