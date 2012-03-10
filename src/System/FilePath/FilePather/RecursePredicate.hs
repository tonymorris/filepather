module System.FilePath.FilePather.RecursePredicate
(
  RecursePredicateT
, RecursePredicate
, recursePredicateT
, recursePredicate
, runRecursePredicateT
, runRecursePredicate
) where

import Control.Monad.Identity

-- | A recurse predicate takes a 'FilePath' and returns whether or not to continue recursing on that file.
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

