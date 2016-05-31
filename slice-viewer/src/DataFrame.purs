module DataFrame (
    DataFrame
  , init
  , empty
  , rows
  , run
  , runOrig
  , filterNone
  , filterAll
  , takeFilter
  , rowFilter
  , groupBy
  , uniqueBy
  , range
  ) 
where

import Prelude
import Data.Array (snoc, filter, take, sortBy, nubBy, length)
import Data.Lazy (Lazy, defer, force)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Map as M

type GroupRow a = {group :: Number, data :: DataFrame a}

data Operation a
  = NoneFilter
  | TakeFilter Int
  | RowFilter (a -> Boolean)
  | Sort (a -> a -> Ordering)
  | Unique (a -> a -> Boolean)

data DataFrame a
  = TopLevel (Array a) 
  | LowerLevel 
      { parent :: DataFrame a
      , operation :: Operation a
      , results :: Lazy (Array a)
      }
  | GroupLevel
      { parent :: DataFrame a
      , results :: Lazy (Array a)
      }

init :: forall a tr. (Foldable tr) => tr a -> DataFrame a
init xs = TopLevel $ foldl snoc [] xs

empty :: forall a. DataFrame a
empty = init []

rows :: forall a. DataFrame a -> Int
rows = length <<< run

run :: forall a. DataFrame a -> Array a
run (TopLevel xs) = xs
run (LowerLevel df) = force df.results
run (GroupLevel df) = force df.results

runOp :: forall a. Operation a -> Array a -> Array a
runOp NoneFilter xs = []
runOp (TakeFilter n) xs = take n xs
runOp (RowFilter f) xs = filter f xs
runOp (Sort f) xs = sortBy f xs
runOp (Unique f) xs = nubBy f xs

runOrig :: forall a. DataFrame a -> Array a
runOrig (TopLevel xs) = xs
runOrig (LowerLevel df) = runOrig df.parent
runOrig (GroupLevel df) = runOrig df.parent

filterNone :: forall a. DataFrame a -> DataFrame a
filterNone = init <<< runOrig

filterAll :: forall a. DataFrame a -> DataFrame a
filterAll p = LowerLevel
  { parent: p
  , operation: NoneFilter
  , results: defer (\_ -> []) -- don't bother running the others
  }

takeFilter :: forall a. Int -> DataFrame a -> DataFrame a
takeFilter n p = LowerLevel
  { parent: p
  , operation: op
  , results: defer (\_ -> runOp op $ run p)
  }
  where 
  op = TakeFilter n

rowFilter :: forall a. (a -> Boolean) -> DataFrame a -> DataFrame a
rowFilter f p = LowerLevel 
  { parent: p
  , operation: op
  , results: defer (\_ -> runOp op $ run p)
  }
  where 
  op = RowFilter f

rowSort :: forall a. (a -> a -> Ordering) -> DataFrame a -> DataFrame a
rowSort f p = LowerLevel
  { parent: p
  , operation: op
  , results: defer (\_ -> runOp op $ run p)
  }
  where op = Sort f

uniqueBy :: forall a. (a -> a -> Boolean) -> DataFrame a -> DataFrame a
uniqueBy f p = LowerLevel
  { parent: p
  , operation: op
  , results: defer (\_ -> runOp op $ run p)
  }
  where op = Unique f

-- TODO: make groups work for all Ords
groupBy :: forall a. (a -> Number) -> DataFrame a -> DataFrame (GroupRow a)
groupBy f p = init $ groups f p
-- FIXME: this is wrong, it needs to be incorporated into the rest of the hierarchy

--| returns the range of applying some function over the data frame
range :: forall a. (a -> Number) -> DataFrame a -> Maybe (Tuple Number Number)
range f df = foldl range' Nothing $ run df
  where
  range' Nothing  x' = Just $ Tuple (f x') (f x')
  range' (Just (Tuple mn mx)) x' = 
    Just $ Tuple (if (f x') < mn then f x' else mn) (if (f x') > mx then f x' else mx)

groups :: forall a. (a -> Number) -> DataFrame a -> Array (GroupRow a)
groups f xs = foldl (\x t -> x `snoc` (convert' xs t)) [] $ M.toList groupMap
  where
  groupMap :: M.Map Number (Array a)
  groupMap = M.fromFoldableWith (++) $ groupIds f $ run xs

convert' :: forall a. DataFrame a -> Tuple Number (Array a) -> GroupRow a
convert' p (Tuple gid vs) = 
  { group: gid
  , data: GroupLevel { parent: p, results: defer (\_ -> vs) }
  }

groupIds :: forall a. (a -> Number) -> Array a -> Array (Tuple Number (Array a))
groupIds f = map (\x -> Tuple (f x) [x])

