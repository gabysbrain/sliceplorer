module Data.Samples where

import Prelude
import Data.Slices (Slice(..), metrics)
import Data.Array (nub, concat, head, snoc)
import Data.Array as A
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(..), either)
import Data.StrMap as SM
import Data.Foldable (foldl)
import Data.Traversable (sequence)
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, get)

import Stats (Histogram, histogram)

-- the slices for a single focus point
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

dims :: SampleGroup -> Int
dims (SampleGroup sg) = 
  case head sg of
       Nothing -> 0
       Just (DimSamples x) -> x.dims

metricHistograms :: Int -> SampleGroup -> Array (SM.StrMap Histogram)
metricHistograms bins sg =
  map (\m -> map (histogram bins) m) flatM
  where
  flatM = flattenMetrics sg

flattenMetrics :: SampleGroup -> Array (SM.StrMap (Array Number))
flattenMetrics (SampleGroup sg) =
  map combineMaps tmp -- TODO: there's probably a prettier way to do all this
  where 
  tmp :: Array (Array (SM.StrMap Number))
  tmp = sequence $ map procDs sg

  procDs :: DimSamples -> Array (SM.StrMap Number)
  procDs (DimSamples ds) = map metrics ds.slices


combineMaps :: Array (SM.StrMap Number) -> SM.StrMap (Array Number)
combineMaps ms = 
  foldl mergeMaps SM.empty ms

mergeMaps :: SM.StrMap (Array Number) -> SM.StrMap Number -> SM.StrMap (Array Number)
mergeMaps out m = 
  SM.fold (\o k v -> SM.alter (merge v) k o) out m

merge :: Number -> Maybe (Array Number) -> Maybe (Array Number)
merge x Nothing = Just [x]
merge x (Just xs) = Just $ snoc xs x


