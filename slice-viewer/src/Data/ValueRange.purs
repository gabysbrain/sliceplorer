module Data.ValueRange where

import Data.Tuple (Tuple, fst, snd)

type ValueRange = Tuple Number Number

minVal :: ValueRange -> Number
minVal = fst

maxVal :: ValueRange -> Number
maxVal = snd

