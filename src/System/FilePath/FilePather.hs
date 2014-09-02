module System.FilePath.FilePather where

import System.FilePath

newtype FilePather f a =
  FilePather (FilePath -> f (FilePath, a))
