module Data.SliceSample where

import Prelude (($))
import Data.Array (concat)
import Data.Samples (SampleGroup(..), FocusPoint, FocusPointInfo(..))
import Data.Slices (Slice(..), Sample, Metrics)
import Util (mapEnum)

data SliceSample = SliceSample
  { focusPointId :: Int
  , clusterId    :: Int
  , d            :: Int
  , focusPoint   :: FocusPoint
  , metrics      :: Metrics
  , slice        :: Array Sample
  }

create :: SampleGroup -> Array SliceSample
create (SampleGroup sg) = 
  concat $ mapEnum (\i fp -> createFromFocusPoint i fp) sg

createFromFocusPoint :: Int -> FocusPointInfo -> Array SliceSample
createFromFocusPoint i (FocusPointInfo fp) =
  mapEnum (\d s -> createFromSlice i d fp.focusPoint s) fp.slices

createFromSlice :: Int -> Int -> Array Number -> Slice -> SliceSample
createFromSlice i dim fp (Slice s) = SliceSample 
  { focusPointId: i
  , clusterId: s.clusterId
  , d: dim
  , focusPoint: fp
  , metrics: s.metrics
  , slice: s.slice
  }


