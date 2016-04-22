module Data.Samples where

import Prelude
import Global (readFloat)
import Data.String
import Data.Array (snoc, slice, length, head, last, take)
import Data.Tuple (Tuple(..))
import Data.Maybe.Unsafe (fromJust)

type Inputs = Array Number
type Output = Number
type Row = Tuple Inputs Output
type Samples = Array Row
type DimSamples = Array Samples
newtype SampleGroup = SampleGroup (Array DimSamples)

numSamples :: Int
numSamples = 21 -- defined by python

rows :: String -> Array String
rows s = split "\n" $ trim s

cols :: String -> Array String
cols = split ","

field :: String -> Number
field s = readFloat $ trim s

rawRows :: String -> Array (Array Number)
rawRows s = map convertFields (rows s)
  where
  convertFields :: String -> Array Number
  convertFields r = map field $ cols r

parse :: String -> SampleGroup
parse s = parse'' $ rawRows s

parse'' :: Array (Array Number) -> SampleGroup
parse'' [] = SampleGroup []
parse'' xs = chunkSamples dims xs
  where
  dims = (length (fromJust $ head xs)) - 1

chunk :: forall a. Int -> Array a -> Array (Array a)
chunk i xs = chunk'' 0 i xs []

chunk'' :: forall a. Int -> Int -> Array a -> Array (Array a) -> Array (Array a)
chunk'' x i xs rest =
  if x >= length xs
     then rest
     else chunk'' (x+i) i xs $ snoc rest (slice x (x+i) xs)

chunkSamples :: Int -> Array (Array Number) -> SampleGroup
chunkSamples d xs = SampleGroup $ map (chunkDims d) $ chunk (d*numSamples) xs

chunkDims :: Int -> Array (Array Number) -> DimSamples
chunkDims d xs = map (createRows d) $ chunk numSamples xs
  
createRows :: Int -> Array (Array Number) -> Samples
createRows d = map (createRow d)

createRow :: Int -> Array Number -> Row
createRow d xs = Tuple (take d xs) (fromJust $ last xs)

