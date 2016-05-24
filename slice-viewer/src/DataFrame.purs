module DataFrame (
    DataFrame
  , init
  , run
  , filterNone
  , takeFilter
  , rowFilter
  ) 
where

import Prelude
import Data.Array (snoc, filter, take)
import Data.Lazy (Lazy, defer, force)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl)

type GroupRow a b = {group :: b, data :: DataFrame a}

data Operation a
  = NoneFilter
  | TakeFilter Int
  | RowFilter (a -> Boolean)
  | Sort (a -> a -> Ordering)

data DataFrame a b
  = TopLevel (Array a) 
  | LowerLevel 
      { parent :: DataFrame a
      , operation :: Operation a
      , results :: Lazy (Array a)
      }
  | GroupLevel
      { parent :: DataFrame a
      , results :: Lazy (Array (GroupRow a b))
      }

init :: forall a tr. (Foldable tr) => tr a -> DataFrame a
init xs = TopLevel $ foldl snoc [] xs

run :: forall a. DataFrame a -> Array a
run (TopLevel xs) = xs
run (LowerLevel df) = force df.results

runOp :: forall a. Operation a -> Array a -> Array a
runOp NoneFilter xs = []
runOp (TakeFilter n) xs = take n xs
runOp (RowFilter f) xs = filter f xs

runOrig :: forall a. DataFrame a -> Array a
runOrig (TopLevel xs) = xs
runOrig (LowerLevel df) = runOrig df.parent

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

groupBy :: (Ord b) => forall a b. (a -> b) -> DataFrame a 
                                           -> DataFrame (GroupRow a b)
groupBy f p = GroupLevel
  { parent: p
  , results: defer (\_ -> groups f $ run p)
  }

groups :: (Ord b) => forall a b. (a -> b) -> Array a -> Array (GroupRow a b)
groups xs = foldl (\x (Tuple k v) -> x `snoc` {group: k, data: v}) [] groupMap
  where
  groupMap = M.fromFoldableWith (++) $ groupIds f xs

groupIds :: (Ord b) => forall a b. (a -> b) -> Array a -> Array (GroupRow a b)
groupIds f = map (\x -> Tuple (f x) [x])

