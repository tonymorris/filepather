module System.FilePath.FilePather.FileType
(
  FileType(..)
, isFileType
, isDirectoryType
, isUnknownType
) where
-- | The possible types of a file.
data FileType =
  File -- ^ The type is a normal file.
  | Directory -- ^ The type is a directory.
  | Unknown -- ^ The type is unknown.
  deriving (Eq, Show, Enum)

isFileType ::
  FileType
  -> Bool
isFileType =
  (==) File

isDirectoryType ::
  FileType
  -> Bool
isDirectoryType =
  (==) Directory

isUnknownType ::
  FileType
  -> Bool
isUnknownType =
  (==) Unknown

