module Data.SliceSample where

import Prelude (($), map)
import Data.Array (concat)
import Data.Samples (SampleGroup(..), FocusPoint, FocusPointInfo(..))
import Data.Slices (Slice(..), Sample, Metrics)
import Data.Tuple (Tuple(..))
import Util (mapEnum, map2)

data SliceSample = SliceSample
  { focusPointId :: Int
  , clusterId    :: Int
  , neighborIds  :: Array Int
  , d            :: Int
  , dimName      :: String
  , focusPoint   :: FocusPoint
  , metrics      :: Metrics
  , slice        :: Array Sample
  }

create :: SampleGroup -> Array SliceSample
create (SampleGroup sg) = 
  concat $ map createFromFocusPoint sg

createFromFocusPoint :: FocusPointInfo -> Array SliceSample
createFromFocusPoint (FocusPointInfo fp) =
  map2 _create (mapEnum Tuple fp.dimNames) fp.slices
  where
  _create (Tuple d dn) s = createFromSlice fp.id d dn fp.neighborIds fp.focusPoint s

createFromSlice :: Int -> Int -> String -> Array Int -> Array Number -> Slice -> SliceSample
createFromSlice i dim dimName ns fp (Slice s) = SliceSample 
  { focusPointId: i
  , clusterId: s.clusterId
  , neighborIds: ns
  , d: dim
  , dimName: dimName
  , focusPoint: fp
  , metrics: s.metrics
  , slice: s.slice
  }


