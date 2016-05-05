module Data.Samples where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Either (Either(..), either)
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Control.Monad.Eff.Exception (Error, error)

type Inputs = Array Number
type Output = Number
type Row = Tuple Inputs Output
newtype Sample = Sample (Tuple Number Number)
data Slice = Slice
  { variance :: Number
  , slice   :: Array Sample
  }
data DimSamples = DimSamples 
  { dims       :: Int
  , focusPoint :: Array Number
  , slices     :: Array Slice
  }
newtype SampleGroup = SampleGroup (Array DimSamples)

instance dimSamplesIsForeign :: IsForeign DimSamples where
  read json = do
    dims <- readProp "dims" json
    fp   <- readProp "slice" json
    s    <- readProp "slices" json
    pure $ DimSamples {dims: dims, focusPoint: fp, slices: s}

instance sampleGroupIsForeign :: IsForeign SampleGroup where
  read json = do
    sg <- read json
    pure $ SampleGroup sg

instance sliceIsForeign :: IsForeign Slice where
  read json = do
    v <- readProp "variance" json
    s <- readProp "slice" json
    pure $ Slice {variance: v, slice: s}

instance sampleIsForeign :: IsForeign Sample where
  read json = do
    x <- readProp "x" json
    y <- readProp "y" json
    pure $ Sample $ Tuple x y

parseJson :: String -> Either Error SampleGroup
parseJson json = case readJSON json of
                      Left err -> Left (error $ show err)
                      Right res -> Right res

