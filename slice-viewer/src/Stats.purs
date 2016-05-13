module Stats where

import Prelude
import Data.Array (span, snoc, cons, sort, length)
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.Maybe.Unsafe (fromJust)

import Debug.Trace

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

type Histogram =
  { min :: Number
  , max :: Number
  , width :: Number
  , numBins :: Int
  , binStarts :: Array Number
  , binEnds :: Array Number
  , counts :: Array Int
  }

type HistBin =
  { start :: Number
  , end :: Number
  , count :: Int
  }

histogram :: Int -> Array Number -> Histogram
histogram numBins nums = 
  { min: mn
  , max: mx
  , width: width
  , numBins: numBins
  , binStarts: map (\x -> x.start) bins
  , binEnds: map (\x -> x.end) bins
  , counts: map (\x -> x.count) bins
  }
  where
  mx = fromJust $ maximum nums
  mn = fromJust $ minimum nums
  width = (mx-mn) / (toNumber numBins)
  bins = h' mn width (sort nums) []

-- nums must be sorted!!!
h' :: Number -> Number -> Array Number -> Array HistBin -> Array HistBin
h' start width nums bins =
  case splotted.init of
       [] -> bins
       xs -> h' (start+width) width splotted.rest $ 
                snoc bins {start: start, end: start+width, count: length xs}
  where
  splotted = span (\x -> x <= (start+width)) nums

