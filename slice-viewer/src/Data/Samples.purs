module Data.Samples where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (sortBy, nub, concat)
import Data.StrMap (StrMap)
import Data.StrMap as SM
import Data.Either (Either(..), either)
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Data.Foreign.Keys (keys)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, get)

type Inputs = Array Number
type Output = Number
type Row = Tuple Inputs Output
newtype Sample = Sample (Tuple Number Number)
data Slice = Slice
  { metrics :: Metrics
  , slice   :: Array Sample
  }
type Metrics = StrMap Number
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

jsonSamples :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) (Either Error SampleGroup)
jsonSamples fname dims = do
  let url = "http://localhost:5000/slice/" ++ fname ++ "/" ++ (show dims)
  --res <- attempt $ get "/data/test.csv"
  res <- attempt $ get url
  let samples = case res of
        Right r  -> parseJson r.response
        Left err -> Left err
  pure $ samples

parseJson :: String -> Either Error SampleGroup
parseJson json = case readJSON json of
                      Left err -> Left (error $ show err)
                      Right res -> Right res

sort :: (DimSamples -> DimSamples -> Ordering) -> SampleGroup -> SampleGroup
sort cmp (SampleGroup sg) = SampleGroup $ sortBy cmp sg

metricNames :: DimSamples -> Array String
metricNames (DimSamples r) = 
  nub $ concat $ map (\(Slice s) -> SM.keys s.metrics) r.slices

