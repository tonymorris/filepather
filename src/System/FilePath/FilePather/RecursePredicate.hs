module System.FilePath.FilePather.RecursePredicate
(
  RecursePredicateT
, RecursePredicate
, recursePredicateT
, recursePredicate
, runRecursePredicateT
, runRecursePredicate
, toFilterPredicate
) where

import Prelude

import Control.Monad.Identity
import System.FilePath.FilePather.FilterPredicate

-- | A recurse predicate takes a 'FilePath', which is a directory, and returns whether or not to continue recursing down on that directory.
newtype RecursePredicateT f =
  RecursePredicateT (FilePath -> f Bool)

-- | A recurse predicate that does not require effects to compute its result.
type RecursePredicate =
  RecursePredicateT Identity

-- | Construct a recurse predicate. The most general construction function.
recursePredicateT ::
  (FilePath -> f Bool)
  -> RecursePredicateT f
recursePredicateT =
  RecursePredicateT

-- | Construct a recurse predicate that does not require effects to compute its result.
recursePredicate ::
  (FilePath -> Bool)
  -> RecursePredicate
recursePredicate f =
  recursePredicateT (Identity . f)

-- | Extract the recurse predicate function.
runRecursePredicateT ::
  RecursePredicateT f
  -> FilePath
  -> f Bool
runRecursePredicateT (RecursePredicateT f) =
  f

-- | Extract the recurse predicate function that does not require effects to compute its result.
runRecursePredicate ::
  RecursePredicate
  -> FilePath
  -> Bool
runRecursePredicate p =
  runIdentity . runRecursePredicateT p

-- | Convert the recurse predicate to a filter predicate.
toFilterPredicate ::
  RecursePredicateT f
  -> FilterPredicateT f
toFilterPredicate (RecursePredicateT f) =
  filterPredicateT (const . f)

