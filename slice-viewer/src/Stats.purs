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

{--instance showHistBin :: Show HistBin where--}
  {--show {start=s, end=e, count=c} = --}
    {--"{start: " ++ (show s) ++ (" end: " ++ (show e) ++ " count: " ++ (show c) ++ "}"--}

histogram :: Int -> Array Number -> Histogram
histogram numBins nums = 
  if width < 1e-9
     then { min: mn
          , max: mx
          , width: mx-mn
          , numBins: 1
          , binStarts: [mn]
          , binEnds: [mx]
          , counts: [length nums]
          } -- FIXME: find a better solution for small difference numbers
     else let bins = h' mn width (sort nums) []
           in { min: mn
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

-- nums must be sorted!!!
h' :: Number -> Number -> Array Number -> Array HistBin -> Array HistBin
h' start width nums bins =
  case splotted of
       {rest=[]} -> bins
       {init=xs, rest=rst} -> h' (start+width) width rst $ 
                                snoc bins {start: start, end: start+width, count: length xs}
  where
  splotted = span (\x -> x <= (start+width)) nums

