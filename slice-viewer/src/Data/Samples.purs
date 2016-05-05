module Data.Samples where

import Prelude
import Data.Slices (Slice(..))
import Data.Array (nub, concat)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.StrMap as SM
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, get)

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

sortBy :: (DimSamples -> DimSamples -> Ordering) -> SampleGroup -> SampleGroup
sortBy cmp (SampleGroup sg) = SampleGroup $ A.sortBy cmp sg

metricNames :: DimSamples -> Array String
metricNames (DimSamples r) = 
  nub $ concat $ map (\(Slice s) -> SM.keys s.metrics) r.slices

sliceList :: SampleGroup -> Array Slice
sliceList (SampleGroup sg) = concat $ map (\(DimSamples ds) -> ds.slices) sg

