module Data.Samples where

import Prelude
import Data.Slices (Slice(..), metrics)
import Data.Array (nub, take, concat, snoc, (!!))
import Data.Array as A
import Data.Foldable as F
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(..))
import Data.StrMap as SM
import Data.Foreign.Class (class IsForeign, read, readJSON, readProp)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, get)
import Util (unsafeJust)
import Control.Monad.Except (runExcept)

import Data.Config (fullUrl)
import Stats (Histogram, histogram)

type FocusPoint = Array Number

-- the slices for a single focus point
data FocusPointInfo = FocusPointInfo 
  { id          :: Int
  , neighborIds :: Array Int
  , dimNames    :: Array String
  , focusPoint  :: FocusPoint
  , slices      :: Array Slice
  }
newtype SampleGroup = SampleGroup (Array FocusPointInfo)

instance focusPointIsForeign :: IsForeign FocusPointInfo where
  read json = do
    i  <- readProp "group_id" json
    ns <- readProp "neighbor_group_ids" json
    dn <- readProp "dim_names" json
    fp <- readProp "slice" json
    s  <- readProp "slices" json
    pure $ FocusPointInfo {id: i, neighborIds: ns, dimNames: dn, focusPoint: fp, slices: s}

instance sampleGroupIsForeign :: IsForeign SampleGroup where
  read json = do
    sg <- read json
    pure $ SampleGroup sg

instance focusPointEq :: Eq FocusPointInfo where
  eq (FocusPointInfo fp1) (FocusPointInfo fp2) = fp1.id == fp2.id

type MetricRangeFilter =
  { metric :: String
  , minVal :: Number
  , maxVal :: Number
  }

jsonSamples :: forall eff
            .  String 
            -> Int 
            -> Aff (ajax :: AJAX | eff) (Either Error SampleGroup)
jsonSamples fname d = do
  let url = fullUrl ("/slice/" <> fname <> "/" <> (show d) <> "/500")
  res <- attempt $ get url
  let samples = case res of
        Right r  -> parseJson r.response
        Left err -> Left err
  pure $ samples

head :: SampleGroup -> Maybe FocusPointInfo
head (SampleGroup sg) = A.head sg

length :: SampleGroup -> Int
length (SampleGroup sg) = A.length sg

parseJson :: String -> Either Error SampleGroup
parseJson json = case runExcept $ readJSON json of
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
dims sg = case head sg of
  Nothing -> 0
  Just (FocusPointInfo x) -> A.length x.dimNames

dimNames :: SampleGroup -> Array String
dimNames sg = case head sg of
  Nothing -> []
  Just (FocusPointInfo x) -> x.dimNames

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
  procDs d (FocusPointInfo ds) = metrics (unsafeJust $ ds.slices !! d)

combineMaps :: Array (SM.StrMap Number) -> SM.StrMap (Array Number)
combineMaps = F.foldMap mergeMaps

mergeMaps :: SM.StrMap Number -> SM.StrMap (Array Number)
mergeMaps = map pure

getFocusPoint :: Int -> SampleGroup -> Maybe FocusPointInfo
getFocusPoint i (SampleGroup sg) = sg !! i

focusPoint :: FocusPointInfo -> Array Number
focusPoint (FocusPointInfo ds) = ds.focusPoint

