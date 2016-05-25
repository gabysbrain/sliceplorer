module DataFrame (
    DataFrame
  , init
  , run
  , runOrig
  , filterNone
  , filterAll
  , takeFilter
  , rowFilter
  , groupBy
  ) 
where

import Prelude
import Data.Array (snoc, filter, take, sortBy)
import Data.Lazy (Lazy, defer, force)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl)
import Data.Map as M

type GroupRow a = {group :: Number, data :: DataFrame a}

data Operation a
  = NoneFilter
  | TakeFilter Int
  | RowFilter (a -> Boolean)
  | Sort (a -> a -> Ordering)

data DataFrame a
  = TopLevel (Array a) 
  | LowerLevel 
      { parent :: DataFrame a
      , operation :: Operation a
      , results :: Lazy (Array a)
      }
  {--| GroupLevel--}
      {--{ parent :: DataFrame a--}
      {--, results :: Lazy (Array (GroupRow a))--}
      {--}--}

init :: forall a tr. (Foldable tr) => tr a -> DataFrame a
init xs = TopLevel $ foldl snoc [] xs

run :: forall a. DataFrame a -> Array a
run (TopLevel xs) = xs
run (LowerLevel df) = force df.results

runOp :: forall a. Operation a -> Array a -> Array a
runOp NoneFilter xs = []
runOp (TakeFilter n) xs = take n xs
runOp (RowFilter f) xs = filter f xs
runOp (Sort f) xs = sortBy f xs

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

-- TODO: make groups work for all Ords
groupBy :: forall a. (a -> Number) -> DataFrame a -> DataFrame (GroupRow a)
groupBy f p = init $ groups f (run p) 
-- FIXME: this is wrong, it needs to be incorporated into the rest of the hierarchy

groups :: forall a. (a -> Number) -> Array a -> Array (GroupRow a)
groups f xs = foldl (\x t -> x `snoc` (convert' t)) [] $ M.toList groupMap
  where
  groupMap :: M.Map Number (Array a)
  groupMap = M.fromFoldableWith (++) $ groupIds f xs

convert' :: forall a. Tuple Number (Array a) -> GroupRow a
convert' (Tuple gid vs) = {group: gid, data: init vs} -- FIXME: shouldn't use init here!

groupIds :: forall a. (a -> Number) -> Array a -> Array (Tuple Number (Array a))
groupIds f = map (\x -> Tuple (f x) [x])

