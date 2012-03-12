module System.FilePath.FilePather.FilePathPredicate
(
  FilePathPredicate(..)
) where

import qualified System.FilePath as P
import System.FilePath.FilePather.RecursePredicate
import System.FilePath.FilePather.FilterPredicate
import Control.Monad
import qualified Data.Foldable as F

-- | Functions that are common to predicates that work on 'FilePath' values.
class FilePathPredicate f where
  -- | A predicate that always succeeds.
  always ::
    Monad g =>
    f g
  -- | A predicate that always fails.
  never ::
    Monad g =>
    f g
  -- | Return a predicate that succeeds only if the two given predicates succeed.
  (.&&.) ::
    Monad g =>
    f g
    -> f g
    -> f g
  -- | Return a predicate that succeeds if any of the two given predicates succeed.
  (.||.) ::
    Monad g =>
    f g
    -> f g
    -> f g
  -- | Negates the given predicate.
  (.!.) ::
    Monad g =>
    f g
    -> f g
  -- | Folds the predicates on disjunction.
  anyof ::
    (F.Foldable t, Monad g) =>
    t (f g)
    -> f g
  -- | Folds the predicates on conjunction.
  anyof =
    F.foldr (.||.) never
  allof ::
    (F.Foldable t, Monad g) =>
    t (f g)
    -> f g
  allof =
    F.foldr (.&&.) always
  -- | A predicate that computes its result based on a file name extension.
  extension ::
    Monad g =>
    (FilePath -> Bool)
    -> f g
  -- | A predicate that computes its result based equivalence to a file name extension. This function matches with and without the preceding extension separator (.).
  extensionEq ::
    Monad g =>
    FilePath
    -> f g
  extensionEq p =
    extension (== p)
  -- | A predicate that computes its result based equivalence to one of a list of file name extensions.
  extensionOneof ::
    (F.Foldable t, Monad g) =>
    t FilePath
    -> f g
  extensionOneof =
    F.foldr (\a b -> extensionEq a .||. b) never
  -- | A predicate that computes its result based inequivalence to any of a list of file name extensions.
  extensionNoneof ::
    (F.Foldable t, Monad g) =>
    t FilePath
    -> f g
  extensionNoneof =
    F.foldr (\a b -> (.!.) (extensionEq a) .&&. b) always
  -- | A predicate that computes its result based on a directory.
  directory ::
    Monad g =>
    (FilePath -> Bool)
    -> f g
  -- | A predicate that succeeds if its 'FilePath' has an extension.
  hasExtension ::
    Monad g =>
    f g
  hasExtension =
    (.!.) notHasExtension
  -- | A predicate that succeeds if its 'FilePath' does not have an extension.
  notHasExtension ::
    Monad g =>
    f g
  notHasExtension =
    (.!.) hasExtension
  -- | A predicate that computes its result based on the splitting of a name and extension.
  splitExtension ::
    Monad g =>
    (String -> String -> Bool)
    -> f g
  -- | A predicate that computes its result based on the splitting of a name into directories.
  splitDirectories ::
    Monad g =>
    ([FilePath] -> Bool)
    -> f g
  -- | A predicate that succeeds if its 'FilePath' has a trailing path separator.
  hasTrailingPathSeparator ::
    Monad g =>
    f g
  hasTrailingPathSeparator =
    (.!.) notHasTrailingPathSeparator
  -- | A predicate that succeeds if its 'FilePath' does not have a trailing path separator.
  notHasTrailingPathSeparator::
    Monad g =>
    f g
  notHasTrailingPathSeparator =
    (.!.) hasTrailingPathSeparator
  -- | A predicate that computes its result based on the file name.
  fileName ::
    Monad g =>
    (FilePath -> Bool)
    -> f g
  -- | A predicate that computes its result based on the base name.
  baseName ::
    Monad g =>
    (FilePath -> Bool)
    -> f g
  -- | A predicate that computes its result based on the normalised file name.
  normalise ::
    Monad g =>
    (FilePath -> Bool)
    -> f g
  -- | A predicate that computes its result based on the file name having been made valid.
  makeValid ::
    Monad g =>
    (FilePath -> Bool)
    -> f g
  -- | A predicate that succeeds if its 'FilePath' is relative.
  isRelative ::
    Monad g =>
    f g
  isRelative =
    (.!.) isNotRelative
  -- | A predicate that succeeds if its 'FilePath' is not relative.
  isNotRelative ::
    Monad g =>
    f g
  isNotRelative =
    (.!.) isRelative
  -- | A predicate that succeeds if its 'FilePath' is absolute.
  isAbsolute ::
    Monad g =>
    f g
  isAbsolute =
    (.!.) isNotAbsolute
  -- | A predicate that succeeds if its 'FilePath' is not absolute.
  isNotAbsolute ::
    Monad g =>
    f g
  isNotAbsolute =
    (.!.) isAbsolute
  -- | A predicate that succeeds if its 'FilePath' is valid.
  isValid ::
    Monad g =>
    f g
  isValid =
    (.!.) isNotValid
  -- | A predicate that succeeds if its 'FilePath' is not valid.
  isNotValid ::
    Monad g =>
    f g
  isNotValid =
    (.!.) isValid

instance FilePathPredicate RecursePredicateT where
  always =
    recursePredicateT . const . return $ True
  never =
    recursePredicateT . const . return $ False
  f .&&. g =
    recursePredicateT $ \p -> do r <- runRecursePredicateT f p
                                 if r
                                   then
                                     runRecursePredicateT g p
                                   else
                                     return False
  f .||. g =
    recursePredicateT $ \p -> do r <- runRecursePredicateT f p
                                 if r
                                   then
                                     return True
                                   else
                                     runRecursePredicateT g p
  (.!.) f =
    recursePredicateT $ liftM not . runRecursePredicateT f
  extension f = 
    recursePredicateT $ return . liftM2 (||) f (f . drop 1) . P.takeExtension
  directory f = 
    recursePredicateT $ return . f . P.takeDirectory
  hasExtension =
    recursePredicateT $ return . P.hasExtension
  splitExtension f =
    recursePredicateT $ return . uncurry f . P.splitExtension
  splitDirectories f = 
    recursePredicateT $ return . f . P.splitDirectories
  hasTrailingPathSeparator =
    recursePredicateT $ return . P.hasTrailingPathSeparator
  fileName f = 
    recursePredicateT $ return . f . P.takeFileName
  baseName f = 
    recursePredicateT $ return . f . P.takeBaseName
  normalise f = 
    recursePredicateT $ return . f . P.normalise
  makeValid f = 
    recursePredicateT $ return . f . P.makeValid
  isRelative =
    recursePredicateT $ return . P.isRelative
  isAbsolute =
    recursePredicateT $ return . P.isAbsolute
  isValid =
    recursePredicateT $ return . P.isValid

instance FilePathPredicate FilterPredicateT where
  always =
    filterPredicateT . const . const . return $ True
  never =
    filterPredicateT . const . const . return $ False
  f .&&. g =
    filterPredicateT $ \p k -> do r <- runFilterPredicateT f p k
                                  if r
                                    then
                                      runFilterPredicateT g p k
                                    else
                                      return False
  f .||. g =
    filterPredicateT $ \p k -> do r <- runFilterPredicateT f p k
                                  if r
                                    then
                                      return True
                                    else
                                      runFilterPredicateT g p k
  (.!.) f =
    filterPredicateT $ \p -> liftM not . runFilterPredicateT f p
  extension f = 
    filterPredicateT $ const . return . liftM2 (||) f (f . drop 1) . P.takeExtension
  directory f = 
    filterPredicateT $ const . return . f . P.takeDirectory
  hasExtension =
    filterPredicateT $ const . return . P.hasExtension
  splitExtension f =
    filterPredicateT $ const . return . uncurry f . P.splitExtension
  splitDirectories f = 
    filterPredicateT $ const . return . f . P.splitDirectories
  hasTrailingPathSeparator =
    filterPredicateT $ const . return . P.hasTrailingPathSeparator
  fileName f = 
    filterPredicateT $ const . return . f . P.takeFileName
  baseName f = 
    filterPredicateT $ const . return . f . P.takeBaseName
  normalise f = 
    filterPredicateT $ const . return . f . P.normalise
  makeValid f = 
    filterPredicateT $ const . return . f . P.makeValid
  isRelative =
    filterPredicateT $ const . return . P.isRelative
  isAbsolute =
    filterPredicateT $ const . return . P.isAbsolute
  isValid =
    filterPredicateT $ const . return . P.isValid

