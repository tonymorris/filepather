module System.FilePath.FilePather.Find
(
  Find(..)
) where

import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Comonad
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
          [FilePath]
          -> Bool
          -> IO [FilePath]
        rkeep d u =
          return (keep u d)
        trkeep ::
          FileType
          -> IO [FilePath]
        trkeep t =
          rkeep [] $ runIdentity (f p t)
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
                              u <- liftM concat $ forM (filter (`notElem` [".", ".."]) t) (find f' r')
                              rkeep u k
                         else
                           rkeep [] k
                  else
                    trkeep Unknown

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
          [FilePath]
          -> Bool
          -> IO [FilePath]
        rkeep d u =
          return (keep u d)
        trkeep ::
          FileType
          -> IO [FilePath]
        trkeep t =
          f p t >>= rkeep []
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
                               u <- liftM concat $ forM (filter (`notElem` [".", ".."]) t) (find f' r')
                               rkeep u k
                          else
                            rkeep [] k
                   else
                     trkeep Unknown

instance Comonad f => Find (IdentityT f) where
  find f r =
    find (filterPredicateT $ \p -> Identity . extract . runFilterPredicateT f p) (recursePredicateT $ Identity . extract . runRecursePredicateT r)

