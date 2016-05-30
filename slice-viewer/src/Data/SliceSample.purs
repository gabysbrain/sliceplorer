module Data.SliceSample where

import Prelude (($), map)
import Data.Array (concat)
import Data.Samples (SampleGroup(..), FocusPoint, FocusPointInfo(..))
import Data.Slices (Slice(..), Sample, Metrics)
import Util (mapEnum)

data SliceSample = SliceSample
  { focusPointId :: Int
  , clusterId    :: Int
  , neighborIds  :: Array Int
  , d            :: Int
  , focusPoint   :: FocusPoint
  , metrics      :: Metrics
  , slice        :: Array Sample
  }

create :: SampleGroup -> Array SliceSample
create (SampleGroup sg) = 
  concat $ map createFromFocusPoint sg

createFromFocusPoint :: FocusPointInfo -> Array SliceSample
createFromFocusPoint (FocusPointInfo fp) =
  mapEnum (\d s -> createFromSlice fp.id d fp.neighborIds fp.focusPoint s) fp.slices

createFromSlice :: Int -> Int -> Array Int -> Array Number -> Slice -> SliceSample
createFromSlice i dim ns fp (Slice s) = SliceSample 
  { focusPointId: i
  , clusterId: s.clusterId
  , neighborIds: ns
  , d: dim
  , focusPoint: fp
  , metrics: s.metrics
  , slice: s.slice
  }


