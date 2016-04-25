module Data.Samples where

import Prelude
import Global (readFloat)
import Data.String
import Data.Array (snoc, slice, length, head, last, take, zipWith, (..), (!!))
import Data.Tuple (Tuple(..))
import Data.Maybe.Unsafe (fromJust)

type Inputs = Array Number
type Output = Number
type Row = Tuple Inputs Output
type Sample = Tuple Number Number
type Samples = Array Sample
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
chunkSamples dims xs = SampleGroup $ map (chunkDims dims) $ chunk (dims*numSamples) xs

chunkDims :: Int -> Array (Array Number) -> DimSamples
chunkDims dims xs = zipWith createSamples (0..(dims-1)) $ chunk numSamples xs
  
createSamples :: Int -> Array (Array Number) -> Samples
createSamples d = map (createSample d)

createSample :: Int -> Array Number -> Sample
createSample d xs = Tuple (fromJust $ xs !! d) (fromJust $ last xs)

createRow :: Int -> Array Number -> Row
createRow d xs = Tuple (take d xs) (fromJust $ last xs)

