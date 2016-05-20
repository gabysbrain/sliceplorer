module Data.Samples where

import Prelude
import Data.Slices (Slice(..), metrics)
import Data.Array (nub, take, concat, head, snoc, (..), (!!))
import Data.Array as A
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.StrMap as SM
import Data.Foldable (foldl)
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, get)

import Stats (Histogram, histogram)

-- the slices for a single focus point
data FocusPoint = FocusPoint 
  { dims       :: Int
  , focusPoint :: Array Number
  , slices     :: Array Slice
  }
newtype SampleGroup = SampleGroup (Array FocusPoint)

instance dimSamplesIsForeign :: IsForeign FocusPoint where
  read json = do
    d  <- readProp "dims" json
    fp <- readProp "slice" json
    s  <- readProp "slices" json
    pure $ FocusPoint {dims: d, focusPoint: fp, slices: s}

instance sampleGroupIsForeign :: IsForeign SampleGroup where
  read json = do
    sg <- read json
    pure $ SampleGroup sg

jsonSamples :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) (Either Error SampleGroup)
jsonSamples fname d = do
  let url = "http://localhost:5000/slice/" ++ fname ++ "/" ++ (show d)
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

sortBy :: (FocusPoint -> FocusPoint -> Ordering) -> SampleGroup -> SampleGroup
sortBy cmp (SampleGroup sg) = SampleGroup $ A.sortBy cmp sg

metricNames :: FocusPoint -> Array String
metricNames (FocusPoint r) = 
  nub $ concat $ map (\(Slice s) -> SM.keys s.metrics) r.slices

sliceList :: SampleGroup -> Array Slice
sliceList (SampleGroup sg) = concat $ map (\(FocusPoint ds) -> ds.slices) sg

subset :: Int -> SampleGroup -> SampleGroup
subset n (SampleGroup sg) = SampleGroup $ take n sg

dims :: SampleGroup -> Int
dims (SampleGroup sg) = 
  case head sg of
       Nothing -> 0
       Just (FocusPoint x) -> x.dims

metricHistograms :: Int -> Int -> SampleGroup -> SM.StrMap Histogram
metricHistograms bins dim sg =
  map (histogram bins) flatM
  where
  flatM = flattenMetrics dim sg

flattenMetrics :: Int -> SampleGroup -> SM.StrMap (Array Number)
flattenMetrics dim sg'@(SampleGroup sg) =
  combineMaps tmp -- TODO: there's probably a prettier way to do all this
  where 
  tmp :: Array (SM.StrMap Number)
  tmp = map (procDs dim) sg

  procDs :: Int -> FocusPoint -> SM.StrMap Number
  procDs d (FocusPoint ds) = metrics (fromJust $ ds.slices !! d)

combineMaps :: Array (SM.StrMap Number) -> SM.StrMap (Array Number)
combineMaps ms = 
  foldl mergeMaps SM.empty ms

mergeMaps :: SM.StrMap (Array Number) -> SM.StrMap Number -> SM.StrMap (Array Number)
mergeMaps out m = 
  SM.fold (\o k v -> SM.alter (merge v) k o) out m

merge :: Number -> Maybe (Array Number) -> Maybe (Array Number)
merge x Nothing = Just [x]
merge x (Just xs) = Just $ snoc xs x

focusPoint :: FocusPoint -> Array Number
focusPoint (FocusPoint ds) = ds.focusPoint

