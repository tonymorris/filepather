module System.FilePath.FilePather.Find
(
  Find(..)
, findi
) where

import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Comonad
import System.FilePath
import System.FilePath.FilePather.RecursePredicate
import System.FilePath.FilePather.FilterPredicate
import System.FilePath.FilePather.FileType
import System.Directory

-- | Finds all files using the given recurse predicate and filter predicate in the given file path.
class Find f where
  find ::
    FilterPredicateT f
    -> RecursePredicateT f
    -> FilePath
    -> IO [FilePath]
  -- | Find files in the current directory.
  findHere ::
    FilterPredicateT f
    -> RecursePredicateT f
    -> IO [FilePath]
  findHere f r =
    getCurrentDirectory >>= find f r

instance Find Identity where
  find f' r' =
    let find' base p =
          let f =
                runFilterPredicateT f'
              r =
                runRecursePredicateT r'
              z ::
                FilePath
              z =
                if null base then p else base </> p
              keep ::
                Bool
                -> [FilePath]
                -> [FilePath]
              keep u =
                if u then (z:) else id
              rkeep ::
                Bool
                -> [FilePath]
                -> IO [FilePath]
              rkeep u d =
                return (keep u d)
              trkeep ::
                FileType
                -> IO [FilePath]
              trkeep =
                flip rkeep [] . runIdentity . f z
          in do fe <- doesFileExist z
                if fe
                  then
                    trkeep File
                  else
                    do de <- doesDirectoryExist z
                       if de
                         then
                           let (Identity k) = f z Directory
                               (Identity l) = r z
                           in if l
                                then
                                  do t <- getDirectoryContents z
                                     u <- forM (filter (`notElem` [".", ".."]) t) (find f' r' . (z </>))
                                     rkeep k (concat u)
                                else
                                  rkeep k []
                         else
                           trkeep Unknown
    in find' []

-- | A specialisation of `find` to the `Identity` monad. Useful in assisting type-inference.
findi ::
  FilterPredicate
  -> RecursePredicate
  -> FilePath
  -> IO [FilePath]
findi =
  find

instance Find IO where
  find f' r' =
    let find' base p =
          let f =
                runFilterPredicateT f'
              r =
                runRecursePredicateT r'
              z ::
                FilePath
              z =
                if null base then p else base </> p
              keep ::
                Bool
                -> [FilePath]
                -> [FilePath]
              keep u =
                if u then (z:) else id
              rkeep ::
                Bool
                -> [FilePath]
                -> IO [FilePath]
              rkeep u d =
                return (keep u d)
              trkeep ::
                FileType
                -> IO [FilePath]
              trkeep t =
                f z t >>= flip rkeep []
          in do fe <- doesFileExist z
                if fe
                  then
                    trkeep File
                  else
                    do de <- doesDirectoryExist z
                       if de
                         then
                           do k <- f z Directory
                              l <- r z
                              if l
                                then
                                  do t <- getDirectoryContents z
                                     u <- forM (filter (`notElem` [".", ".."]) t) (find f' r' . (z </>))
                                     rkeep k (concat u)
                                else
                                  rkeep k []
                         else
                           trkeep Unknown
    in find' []

instance Comonad f => Find (IdentityT f) where
  find f r =
    find (filterPredicateT $ \p -> Identity . extract . runFilterPredicateT f p) (recursePredicateT $ Identity . extract . runRecursePredicateT r)
