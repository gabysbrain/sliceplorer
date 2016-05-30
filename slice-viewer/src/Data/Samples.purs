module Data.Samples where

import Prelude
import Data.Slices (Slice(..), metrics)
import Data.Array (nub, take, concat, head, snoc, (!!))
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..))
import Data.StrMap as SM
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, get)

import Stats (Histogram, histogram)

type FocusPoint = Array Number

-- the slices for a single focus point
data FocusPointInfo = FocusPointInfo 
  { id          :: Int
  , neighborIds :: Array Int
  , dims        :: Int
  , focusPoint  :: FocusPoint
  , slices      :: Array Slice
  }
newtype SampleGroup = SampleGroup (Array FocusPointInfo)

instance focusPointIsForeign :: IsForeign FocusPointInfo where
  read json = do
    i  <- readProp "group_id" json
    ns <- readProp "neighbor_group_ids" json
    d  <- readProp "dims" json
    fp <- readProp "slice" json
    s  <- readProp "slices" json
    pure $ FocusPointInfo {id: i, neighborIds: ns, dims: d, focusPoint: fp, slices: s}

instance sampleGroupIsForeign :: IsForeign SampleGroup where
  read json = do
    sg <- read json
    pure $ SampleGroup sg

instance focusPointEq :: Eq FocusPointInfo where
  eq (FocusPointInfo fp1) (FocusPointInfo fp2) = fp1.id == fp2.id
    --fp1.dims == fp2.dims && fp1.focusPoint == fp2.focusPoint && fp1.slices == fp2.slices

type MetricRangeFilter =
  { metric :: String
  , minVal :: Number
  , maxVal :: Number
  }

jsonSamples :: forall eff. String -> Int -> Int -> Aff (ajax :: AJAX | eff) (Either Error SampleGroup)
jsonSamples fname d n = do
  let url = "http://localhost:5000/slice/" ++ fname ++ "/" ++ (show d) ++ "/" ++ (show n)
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

sortBy :: (FocusPointInfo -> FocusPointInfo -> Ordering) -> SampleGroup -> SampleGroup
sortBy cmp (SampleGroup sg) = SampleGroup $ A.sortBy cmp sg

metricNames :: FocusPointInfo -> Array String
metricNames (FocusPointInfo r) = 
  nub $ concat $ map (\(Slice s) -> SM.keys s.metrics) r.slices

sliceList :: SampleGroup -> Array Slice
sliceList (SampleGroup sg) = concat $ map (\(FocusPointInfo ds) -> ds.slices) sg

subset :: Int -> SampleGroup -> SampleGroup
subset n (SampleGroup sg) = SampleGroup $ take n sg

dims :: SampleGroup -> Int
dims (SampleGroup sg) = 
  case head sg of
       Nothing -> 0
       Just (FocusPointInfo x) -> x.dims

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

  procDs :: Int -> FocusPointInfo -> SM.StrMap Number
  procDs d (FocusPointInfo ds) = metrics (fromJust $ ds.slices !! d)

combineMaps :: Array (SM.StrMap Number) -> SM.StrMap (Array Number)
combineMaps ms = 
  foldl mergeMaps SM.empty ms

mergeMaps :: SM.StrMap (Array Number) -> SM.StrMap Number -> SM.StrMap (Array Number)
mergeMaps out m = 
  SM.fold (\o k v -> SM.alter (merge v) k o) out m

merge :: Number -> Maybe (Array Number) -> Maybe (Array Number)
merge x Nothing = Just [x]
merge x (Just xs) = Just $ snoc xs x

getFocusPoint :: Int -> SampleGroup -> Maybe FocusPointInfo
getFocusPoint i (SampleGroup sg) = sg !! i

focusPoint :: FocusPointInfo -> Array Number
focusPoint (FocusPointInfo ds) = ds.focusPoint

