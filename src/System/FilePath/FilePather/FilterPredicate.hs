module System.FilePath.FilePather.FilterPredicate
(
  FilterPredicateT
, FilterPredicate
, filterPredicateT
, filterPredicate
, filterPredicateT'
, filterPredicate'
, runFilterPredicateT
, runFilterPredicate
, isDirectoryType
, isFileType
, isUnknownType
) where

import Prelude
import Control.Monad.Identity
import System.FilePath.FilePather.FileType

newtype FilterPredicateT f =
  FilterPredicateT (FilePath -> FileType -> f Bool)

-- | A filter predicate that does not require effects to compute its result.
type FilterPredicate =
  FilterPredicateT Identity

-- | A filter predicate takes a 'FilePath' and a file type and returns whether or not to filter the value.
filterPredicateT ::
  (FilePath -> FileType -> f Bool)
  -> FilterPredicateT f
filterPredicateT =
  FilterPredicateT

-- | Construct a filter predicate that does not require effects to compute its result.
filterPredicate ::
  (FilePath -> FileType -> Bool)
  -> FilterPredicate
filterPredicate f =
  filterPredicateT (\z -> Identity . f z)

-- | A filter predicate takes a 'FilePath' and returns whether or not to filter the value.
filterPredicateT' ::
  (FilePath -> f Bool)
  -> FilterPredicateT f
filterPredicateT' f =
  filterPredicateT (const . f)

-- | Construct a filter predicate that does not require effects to compute its result.
filterPredicate' ::
  (FilePath -> Bool)
  -> FilterPredicate
filterPredicate' f =
  filterPredicate (const . f)

-- | Extract the filter predicate function.
runFilterPredicateT ::
  FilterPredicateT f
  -> FilePath
  -> FileType
  -> f Bool
runFilterPredicateT (FilterPredicateT f) =
  f

-- | Construct a filter predicate that does not require effects to compute its result.
runFilterPredicate ::
  FilterPredicate
  -> FilePath
  -> FileType
  -> Bool
runFilterPredicate p k =
  runIdentity . runFilterPredicateT p k

isDirectoryType ::
  Monad f =>
  FilterPredicateT f
isDirectoryType =
  filterPredicateT . const $ return . isDirectory

isFileType ::
  Monad f =>
  FilterPredicateT f
isFileType =
  filterPredicateT . const $ return . isFile

isUnknownType ::
  Monad f =>
  FilterPredicateT f
isUnknownType =
  filterPredicateT . const $ return . isUnknown
