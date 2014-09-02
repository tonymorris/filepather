module System.FilePath.FilePather.FileType
(
  FileType(..)
, isFile
, isDirectory
, isUnknown
) where

import Prelude

-- | The possible types of a file.
data FileType =
  File -- ^ The type is a normal file.
  | Directory -- ^ The type is a directory.
  | Unknown -- ^ The type is unknown.
  deriving (Eq, Ord, Show, Enum)

isFile ::
  FileType
  -> Bool
isFile =
  (==) File

isDirectory ::
  FileType
  -> Bool
isDirectory =
  (==) Directory

isUnknown ::
  FileType
  -> Bool
isUnknown =
  (==) Unknown

