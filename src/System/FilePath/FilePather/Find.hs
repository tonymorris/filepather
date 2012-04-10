module System.FilePath.FilePather.Find
(
  Find(..)
, findi
, findpi
, FindR
, foundR
, dropR
, recurseR
, noRecurseR
, foundL
, dropL
, recurseL
, noRecurseL
) where

import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Comonad
import Control.Comonad.Trans.Store
import Data.Lens.Partial.Common hiding (null)
import System.FilePath
import System.FilePath.FilePather.RecursePredicate
import System.FilePath.FilePather.FilterPredicate
import System.FilePath.FilePather.FileType
import System.Directory

class Find f where
  -- | Finds all files using the given recurse predicate and filter predicate in the given file path.
  find ::
    FilterPredicateT f
    -> RecursePredicateT f
    -> FilePath
    -> IO [FindR]
  -- | Find files in the current directory.
  findHere ::
    FilterPredicateT f
    -> RecursePredicateT f
    -> IO [FindR]
  findHere f r =
    getCurrentDirectory >>= find f r
  -- | Finds all files using the given recurse predicate and filter predicate in the given file path.
  findp ::
    FilterPredicateT f
    -> RecursePredicateT f
    -> FilePath
    -> IO [FilePath]
  findp f r =
    liftM (\x -> x >>= \w ->
      case w of
        Found p _   -> [p]
        Drop _ _    -> []
        Recurse _   -> []
        NoRecurse _ -> []) . find f r
  -- | Find files in the current directory.
  findpHere ::
    FilterPredicateT f
    -> RecursePredicateT f
    -> IO [FilePath]
  findpHere f r =
    getCurrentDirectory >>= findp f r

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
                -> FileType
                -> FindR
              keep u =
                (if u then Found else Drop) z
              rkeep ::
                Bool
                -> FindR
              rkeep u =
                (if u then Recurse else NoRecurse) z
              tkeep ::
                FileType
                -> [FindR]
              tkeep t =
                [keep (runIdentity (f z t)) t]
          in do fe <- doesFileExist z
                if fe
                  then
                    return (tkeep File)
                  else
                    do de <- doesDirectoryExist z
                       if de
                         then
                           let (Identity k) = f z Directory
                               (Identity l) = r z
                           in liftM ([rkeep l, keep k Directory] ++) $
                                if l
                                  then
                                    do t <- getDirectoryContents z
                                       u <- forM (filter (`notElem` [".", ".."]) t) (find f' r' . (z </>))
                                       return (concat u)
                                  else
                                    return []
                         else
                           return (tkeep Unknown)
    in find' []

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
                -> FileType
                -> FindR
              keep u =
                (if u then Found else Drop) z
              rkeep ::
                Bool
                -> FindR
              rkeep u =
                (if u then Recurse else NoRecurse) z
              tkeep ::
                FileType
                -> IO [FindR]
              tkeep t =
                f z t >>= \y -> return [keep y t]
          in do fe <- doesFileExist z
                if fe
                  then
                    tkeep File
                  else
                    do de <- doesDirectoryExist z
                       if de
                         then
                           do k <- f z Directory
                              l <- r z
                              liftM ([rkeep l, keep k Directory] ++) $
                                if l
                                  then
                                    do t <- getDirectoryContents z
                                       u <- forM (filter (`notElem` [".", ".."]) t) (find f' r' . (z </>))
                                       return (concat u)
                                  else
                                    return []
                         else
                           tkeep Unknown
    in find' []

instance Comonad f => Find (IdentityT f) where
  find f r =
    find (filterPredicateT $ \p -> Identity . extract . runFilterPredicateT f p) (recursePredicateT $ Identity . extract . runRecursePredicateT r)

-- | A specialisation of `find` to the `Identity` monad. Useful in assisting type-inference.
findi ::
  FilterPredicate
  -> RecursePredicate
  -> FilePath
  -> IO [FindR]
findi =
  find

-- | A specialisation of `findp` to the `Identity` monad. Useful in assisting type-inference.
findpi ::
  FilterPredicate
  -> RecursePredicate
  -> FilePath
  -> IO [FilePath]
findpi =
  findp

-- | The results of a path find. One of
--
-- * @found@ with the file path name and file type.
--
-- * @drop@ with the file path name and file type.
--
-- * @recurse@ with the file path (the file type is always directory).
--
-- * @no-recurse@ with the file path (the file type is always directory).
data FindR =
  Found FilePath FileType
  | Drop FilePath FileType
  | Recurse FilePath
  | NoRecurse FilePath
  deriving (Eq, Show)

foundR ::
  FilePath
  -> FileType
  -> FindR
foundR =
  Found

dropR ::
  FilePath
  -> FileType
  -> FindR
dropR =
  Drop

recurseR ::
  FilePath
  -> FindR
recurseR =
  Recurse

noRecurseR ::
  FilePath
  -> FindR
noRecurseR =
  NoRecurse

foundL ::
  PartialLens FindR (FilePath, FileType)
foundL =
  PLens $ \r ->
    case r of
      Found p t -> Just (store (uncurry Found) (p, t))
      _         -> Nothing

dropL ::
  PartialLens FindR (FilePath, FileType)
dropL =
  PLens $ \r ->
    case r of
      Drop p t -> Just (store (uncurry Drop) (p, t))
      _        -> Nothing

recurseL ::
  PartialLens FindR FilePath
recurseL =
  PLens $ \r ->
    case r of
      Recurse p -> Just (store Recurse p)
      _         -> Nothing

noRecurseL ::
  PartialLens FindR FilePath
noRecurseL =
  PLens $ \r ->
    case r of
      NoRecurse p -> Just (store NoRecurse p)
      _           -> Nothing
