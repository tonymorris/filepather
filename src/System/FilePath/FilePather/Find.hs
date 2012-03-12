module System.FilePath.FilePather.Find
(
  findT
, findHereT
, find
, findHere
) where

import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Comonad
import Data.Distributive
import System.FilePath.FilePather.RecursePredicate
import System.FilePath.FilePather.FilterPredicate
import System.FilePath.FilePather.FileType
import System.Directory

-- | Finds all files using the given recurse predicate and filter predicate in the given file path.
findT ::
  Monad f =>
  FilterPredicateT f
  -> RecursePredicateT f
  -> FilePath
  -> f (IO [FilePath])
findT =
  undefined

-- | Find files in the current directory.
findHereT ::
  (Monad f, Distributive f) =>
  FilterPredicateT f
  -> RecursePredicateT f
  -> f (IO [FilePath])
findHereT f r =
  liftM join (collectM (findT f r) getCurrentDirectory)

find ::
  FilterPredicate
  -> RecursePredicate
  -> FilePath
  -> IO [FilePath]
find f r p =
  runIdentity (findT f r p)

findHere ::
  FilterPredicate
  -> RecursePredicate
  -> IO [FilePath]
findHere f r =
  runIdentity (findHereT f r)

{-
instance Find Identity where
  find f' r' p = 
    let f =
          runFilterPredicateT f'
        r =
          runRecursePredicateT r'
        keep ::
          Bool
          -> [FilePath]
          -> [FilePath]
        keep u =
          if u then (p:) else id
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
          flip rkeep [] . runIdentity . f p
   in do fe <- doesFileExist p
         if fe
           then
             trkeep File
           else
             do de <- doesDirectoryExist p
                if de
                  then
                    let (Identity k) = f p Directory
                        (Identity l) = r p
                    in if l
                         then
                           do t <- getDirectoryContents p
                              u <- forM (filter (`notElem` [".", ".."]) t) (find f' r')
                              rkeep k (concat u)
                         else
                           rkeep k []
                  else
                    trkeep Unknown

-- | A specialisation of `find` to the `Identity` monad. Useful in assisting type-inference.
findi ::
  FilterPredicate
  -> RecursePredicate
  -> FilePath
  -> IO [FilePath]
findi =
  find

instance Find IO where
  find f' r' p =
    let f =
          runFilterPredicateT f'
        r =
          runRecursePredicateT r'
        keep ::
          Bool
          -> [FilePath]
          -> [FilePath]
        keep u =
          if u then (p:) else id
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
          f p t >>= flip rkeep []
    in do fe <- doesFileExist p
          if fe
            then
              trkeep File
            else
              do de <- doesDirectoryExist p
                 if de
                   then
                     do k <- f p Directory
                        l <- r p
                        if l
                          then
                            do t <- getDirectoryContents p
                               u <- forM (filter (`notElem` [".", ".."]) t) (find f' r')
                               rkeep k (concat u)
                          else
                            rkeep k []
                   else
                     trkeep Unknown

instance Comonad f => Find (IdentityT f) where
  find f r =
    find (filterPredicateT $ \p -> Identity . extract . runFilterPredicateT f p) (recursePredicateT $ Identity . extract . runRecursePredicateT r)

-}
