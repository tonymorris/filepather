{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.FilePath.FilePather.Find(
  findFiles
, always
, findFilesAlways
) where

import Control.Applicative ( Applicative(liftA2, pure) )
import Control.Category ( Category(id) )
import Control.Lens ( view )
import Control.Monad ( join, Monad((>>=)) )
import System.FilePath.FilePather.Posix
    ( (</>), FilePath, dropTrailingPathSeparator )
import System.IO ( IO )
import Data.Bool ( Bool(True), bool )
import Data.Function(($))
import Data.Functor ( Functor(fmap) )
import Data.Traversable ( Traversable(traverse) )
import Data.Semigroup ( Semigroup((<>)) )
import System.Directory(doesDirectoryExist, listDirectory)
import System.FilePath.FilePather.ReadFilePath
    ( ReadFilePathT(..), readFilePath )

findFiles ::
  ReadFilePathT IO Bool
  -> ReadFilePathT IO [FilePath]
findFiles (ReadFilePathT test) =
  let bool' ::
        Monad f =>
        f a
        -> f a
        -> f Bool
        -> f a
      bool' f t p =
        p >>= bool f t
      partitionM ::
        Applicative m =>
        (a -> m Bool)
        -> [a]
        -> m ([a], [a])
      partitionM _ [] =
        pure ([], [])
      partitionM f (x:xs) =
        liftA2
          (\res (as, bs) ->
            let onres p q =
                  bool p q res
            in  (onres id (x:) as, onres (x:) id bs)
          )
          (f x)
          (partitionM f xs)
      findFiles' base dx =
        bool'
          (pure [])
          (
            let findFiles'' dir =
                  let dir' =
                        base </> dir
                  in  do  (dirs,files) <- listDirectory dir' >>= partitionM (\d -> doesDirectoryExist (dir' </> d))
                          rest <- fmap join (traverse (\d -> findFiles'' (dir </> d)) dirs)
                          pure (fmap (dir </>) files <> rest)
            in  findFiles'' dx
          )
          (test $ view readFilePath dropTrailingPathSeparator dx)
  in  ReadFilePathT (`findFiles'` "")

always ::
  Applicative f =>
  ReadFilePathT f Bool
always =
  ReadFilePathT (pure (pure True))

findFilesAlways ::
  ReadFilePathT IO [FilePath]
findFilesAlways =
  findFiles always
