module Stats where

import Prelude
import Data.Array (span, snoc, cons, sort, length, zip, filter, head, last)
import Data.Foldable (sum, maximum, minimum)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Util (unsafeJust)

import Data.ValueRange (ValueRange, minVal, maxVal)

type Histogram =
  { min :: Number
  , max :: Number
  , width :: Number
  , numBins :: Int
  , binStarts :: Array Number
  , binEnds :: Array Number
  , counts :: Array Int
  , percentages :: Array Number
  }

type HistBin =
  { start :: Number
  , end :: Number
  , count :: Int
  , percentage :: Number
  }

{--instance showHistBin :: Show HistBin where--}
  {--show {start=s, end=e, count=c} = --}
    {--"{start: " ++ (show s) ++ (" end: " ++ (show e) ++ " count: " ++ (show c) ++ "}"--}

histogram :: Int -> Array Number -> Histogram
histogram numBins nums = histogram' (binRanges numBins nums) nums

histogram' :: Array ValueRange -> Array Number -> Histogram
histogram' binSpecs nums =
  { min: (unsafeJust $ head bins).start
  , max: (unsafeJust $ last bins).end
  , width: (unsafeJust $ head bins).end - (unsafeJust $ head bins).start
  , numBins: length binSpecs
  , binStarts: map (\x -> x.start) bins
  , binEnds: map (\x -> x.end) bins
  , counts: map (\x -> x.count) bins
  , percentages: map (\x -> (toNumber x.count) / ttlCount) bins
  }
  where
  binCount rng = length $ filter (\x -> x >= minVal rng && x <= maxVal rng) nums
  bins = map (\rng -> {start: minVal rng, end: maxVal rng, count: binCount rng}) binSpecs
  ttlCount = toNumber $ sum $ map (_.count) bins

binRanges :: Int -> Array Number -> Array ValueRange
binRanges numBins nums =
  if width < 1e-9 
     then [Tuple mn mx] 
     else zip (range' numBins mn width []) (range' numBins (mn+width) width [])
  where
  mx = unsafeJust $ maximum nums
  mn = unsafeJust $ minimum nums
  width = (mx-mn) / (toNumber numBins)

histRanges :: Histogram -> Array ValueRange
histRanges hist = zip hist.binStarts hist.binEnds

range' :: Int -> Number -> Number -> Array Number -> Array Number
range' n start width nums 
  | n < 1     = nums
  | otherwise = range' (n-1) (start+width) width $ nums `snoc` start


