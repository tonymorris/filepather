module System.FilePath.FilePather
(
  -- * Data type
  FilePather,
  -- * Constructor and unwrapper
  (<?>),
  filePather,
  -- * 'FilePather' values
  filePath,
  always,
  always',
  never,
  never',
  extension,
  extension',
  directory,
  directory',
  hasExtension,
  hasExtension',
  splitExtension,
  splitExtension',
  splitDirectories,
  splitDirectories',
  hasTrailingPathSeparator,
  hasTrailingPathSeparator',
  fileName,
  fileName',
  baseName,
  baseName',
  normalise,
  normalise',
  makeValid,
  makeValid',
  isRelative,
  isRelative',
  isAbsolute,
  isAbsolute',
  isValid,
  isValid',
  -- * Find predicates
  FileType(..),
  RecursePredicate,
  FilterPredicate,
  isFile,
  isDirectory,
  isUnknown,
  -- * Find
  find,
  -- * Combinators
  extensionSatisfies,
  extensionOneOf,
  extensionEq,
  findHere
) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.FilePath((</>), takeExtension, takeDirectory, takeFileName, takeBaseName)
import qualified System.FilePath as P
import System.Directory

-- | A function that takes a 'FilePath' and produces a value.
newtype FilePather a = FilePather {
  (<?>) :: FilePath -> a
}

instance Functor FilePather where
  fmap f (FilePather k) =
    FilePather (f . k)

instance Applicative FilePather where
  FilePather f <*> FilePather a =
    FilePather (f <*> a)
  pure =
    FilePather . const

instance Monad FilePather where
  FilePather f >>= k =
    FilePather (f >>= (<?>) . k)
  return =
    pure

instance (Monoid a) => Monoid (FilePather a) where
  mempty =
    return mempty
  FilePather x `mappend` FilePather y =
    FilePather (x `mappend` y)

-- | Construct a 'FilePather' from the given function
filePather ::
  (FilePath -> a)
  -> FilePather a
filePather =
  FilePather

-- | A value that runs the identity function.
filePath ::
  FilePather FilePath
filePath =
  filePather id

-- | A value that always produces the value 'True'.
always ::
  FilePather Bool
always =
  filePather (const True)

-- | A value using a constant function that produces the value 'True'.
always' ::
  FilePather (a -> Bool)
always' =
  constant always

-- | A value that always produces the value 'False'.
never ::
  FilePather Bool
never =
  filePather (const False)

-- | A value that always produces a constant function that produces the value 'False'.
never' ::
  FilePather (a -> Bool)
never' =
  constant never

-- | A value that produces the extension of the given 'FilePath'.
extension ::
  FilePather FilePath
extension =
  filePather takeExtension

-- | A value using a constant function that produces the extension of the given 'FilePath'.
extension' ::
  FilePather (a -> FilePath)
extension' =
  constant extension

-- | A value that produces the directory of the given 'FilePath'.
directory ::
  FilePather FilePath
directory =
  filePather takeDirectory

-- | A value using a constant function that produces the directory of the given 'FilePath'.
directory' ::
  FilePather (a -> FilePath)
directory' =
  constant directory

-- | A value that produces a value denoting whether or not the given 'FilePath' has an extension.
hasExtension ::
  FilePather Bool
hasExtension =
  filePather P.hasExtension

-- | A value using a constant function that produces a value denoting whether or not the given 'FilePath' has an extension.
hasExtension' ::
  FilePather (a -> Bool)
hasExtension' =
  constant hasExtension

-- | A value that produces a value splitting the given 'FilePath' by its extension.
splitExtension ::
  FilePather (String, String)
splitExtension =
  filePather P.splitExtension

-- | A value using a constant function that produces a value splitting the given 'FilePath' by its extension.
splitExtension' ::
  FilePather (a -> (String, String))
splitExtension' =
  constant splitExtension

-- | A value that produces a value splitting the given 'FilePath' into its directories.
splitDirectories ::
  FilePather [FilePath]
splitDirectories =
  filePather P.splitDirectories

-- | A value using a constant function that produces a value splitting the given 'FilePath' into its directories.
splitDirectories' ::
  FilePather (a -> [FilePath])
splitDirectories' =
  constant splitDirectories

-- | A value that produces a value denoting whether or not the given 'FilePath' has a trailing path separator.
hasTrailingPathSeparator ::
  FilePather Bool
hasTrailingPathSeparator =
  filePather P.hasTrailingPathSeparator

-- | A value using a constant function that produces a value denoting whether or not the given 'FilePath' has a trailing path separator.
hasTrailingPathSeparator' ::
  FilePather (a -> Bool)
hasTrailingPathSeparator' =
  constant hasTrailingPathSeparator

-- | A value that produces the file name of the given 'FilePath'.
fileName ::
  FilePather FilePath
fileName =
  filePather takeFileName

-- | A value using a constant function that produces the file name of the given 'FilePath'.
fileName' ::
  FilePather (a -> FilePath)
fileName' =
  constant fileName

-- | A value that produces the base name of the given 'FilePath'.
baseName ::
  FilePather FilePath
baseName =
  filePather takeBaseName

-- | A value using a constant function that produces the base name of the given 'FilePath'.
baseName' ::
  FilePather (a -> FilePath)
baseName' =
  constant baseName

-- | A value that normalises the given 'FilePath'.
normalise ::
  FilePather FilePath
normalise =
  filePather P.normalise

-- | A value using a constant function that normalises the given 'FilePath'.
normalise' ::
  FilePather (a -> FilePath)
normalise' =
  constant normalise

-- | A value that makes valid the given 'FilePath'.
makeValid ::
  FilePather FilePath
makeValid =
  filePather P.makeValid

-- | A value using a constant function that makes valid the given 'FilePath'.
makeValid' ::
  FilePather (a -> FilePath)
makeValid' =
  constant makeValid

-- | A value that produces a value denoting whether or not the given 'FilePath' has is relative.
isRelative ::
  FilePather Bool
isRelative =
  filePather P.isRelative

-- | A value using a constant function that produces a value denoting whether or not the given 'FilePath' has is relative.
isRelative' ::
  FilePather (a -> Bool)
isRelative' =
  constant isRelative

-- | A value that produces a value denoting whether or not the given 'FilePath' has is absolute.
isAbsolute ::
  FilePather Bool
isAbsolute =
  filePather P.isAbsolute

-- | A value using a constant function that produces a value denoting whether or not the given 'FilePath' has is absolute.
isAbsolute' ::
  FilePather (a -> Bool)
isAbsolute' =
  constant isAbsolute

-- | A value that produces a value denoting whether or not the given 'FilePath' has is valid.
isValid ::
  FilePather Bool
isValid =
  filePather P.isValid

-- | A value using a constant function that produces a value denoting whether or not the given 'FilePath' has is valid.
isValid' ::
  FilePather (a -> Bool)
isValid' =
  constant isValid

-- | The possible types of a file.
data FileType = File -- ^ The type is a normal file.
                | Directory -- ^ The type is a directory.
                | Unknown -- ^ The type is unknown.
  deriving (Eq, Show)

-- | A recurse predicate takes a 'FilePath' and returns whether or not to continue recursing on that file.
type RecursePredicate = FilePather Bool

-- | A filter predicate takes a 'FilePath' and a file type and returns whether or not to filter the value.
type FilterPredicate = FilePather (FileType -> Bool)

-- | Compares for equivalence to a 'File' in an applicative functor.
isFile ::
  Applicative f =>
  f (FileType -> Bool)
isFile =
  pure (== File)

-- | Compares for equivalence to a 'Directory' in an applicative functor.
isDirectory ::
  Applicative f =>
  f (FileType -> Bool)
isDirectory =
  pure (== Directory)

-- | Compares for equivalence to 'Unknown' in an applicative functor.
isUnknown ::
  Applicative f =>
  f (FileType -> Bool)
isUnknown =
  pure (== Unknown)

-- | Finds all files using the given recurse predicate and filter predicate in the given file path.
find ::
  RecursePredicate -- ^ The recurse predicate determines whether to continue recursing on the given file path.
  -> FilterPredicate -- ^ The filter predicate determines whether to keep the current file path.
  -> FilePath -- ^ The file path to begin finding files.
  -> IO [FilePath] -- ^ All files found.
find =
  find' []
    where
    find' :: FilePath -> RecursePredicate -> FilterPredicate -> FilePath -> IO [FilePath]
    find' k r x p = let z = if null k then p else k </> p
                        z' t = [z | x <?> z $ t]
                        ifM c t f = do c' <- c
                                       t' <- t
                                       f' <- f
                                       return (if c' then t' else f')
                    in ifM (doesFileExist z)
                         (return (z' File)) $
                         do e <- doesDirectoryExist z
                            if e
                              then if r <?> z
                                     then do c <- getDirectoryContents z
                                             t <- fmap join $ forM (filter (`notElem` [".", ".."]) c) (find' k r x . (z </>))
                                             return (z' Directory ++ t)
                                     else return (z' Directory)
                              else return (z' Unknown)

-- | Returns a filter predicate based on whether a file extension satisfies a predicate.
extensionSatisfies ::
  (FilePath -> Bool)
  -> FilterPredicate
extensionSatisfies f =
  (const . f) <$> extension

-- | Returns a filter predicate based on whether a file extension is one of the given list of extensions.
extensionOneOf ::
  [FilePath]
  -> FilterPredicate
extensionOneOf =
  extensionSatisfies . flip elem . map ('.':)

-- | Returns a filter predicate based on whether a file extension equals the given extension.
extensionEq ::
  FilePath
  -> FilterPredicate
extensionEq =
  extensionOneOf . return

-- | Find in the current directory.
findHere ::
  RecursePredicate -- ^ The recurse predicate determines whether to continue recursing on the given file path.
  -> FilterPredicate -- ^ The filter predicate determines whether to keep the current file path.
  -> IO [FilePath] -- ^ All files found.
findHere r x =
  find r x =<< getCurrentDirectory

-- not exported
constant ::
  Functor f =>
  f a
  -> f (t -> a)
constant =
  fmap const

