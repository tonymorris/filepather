{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.FilePath.FilePather.Posix(
  splitExtension
, takeExtension
, replaceExtension
, dropExtension
, addExtension
, hasExtension
, splitExtensions
, dropExtensions
, takeExtensions
, replaceExtensions
, isExtensionOf
, stripExtension
, splitFileName
, takeFileName
, replaceFileName
, dropFileName
, takeBaseName
, replaceBaseName
, takeDirectory
, replaceDirectory
, combine
, splitPath
, joinPath
, splitDirectories
, splitDrive
, joinDrive
, takeDrive
, hasDrive
, dropDrive
, isDrive
, hasTrailingPathSeparator
, addTrailingPathSeparator
, dropTrailingPathSeparator
, normalise
, equalFilePath
, makeRelative
, isRelative
, isAbsolute
, isValid
, makeValid
, module SFP
) where

import Control.Applicative ( Applicative )
import Control.Category((.))
import Control.Lens ( (#) )
import Data.String( String )
import Data.Bool( Bool )
import Data.Maybe ( Maybe )
import qualified System.FilePath as FP
import System.FilePath as SFP(FilePath, pathSeparator, pathSeparators, isPathSeparator, extSeparator, isExtSeparator, splitSearchPath, (-<.>), (</>))
import System.FilePath.FilePather.ToFilePath
    ( ToFilePath, toFilePath )
import System.FilePath.FilePather.ReadFilePath
    ( ReadFilePathT, liftReadFilePath )

splitExtension ::
  Applicative f =>
  ReadFilePathT f (String, String)
splitExtension =
  liftReadFilePath FP.splitExtension

takeExtension ::
  Applicative f =>
  ReadFilePathT f String
takeExtension =
  liftReadFilePath FP.takeExtension

replaceExtension ::
  Applicative f =>
  ReadFilePathT f (String -> FilePath)
replaceExtension =
  liftReadFilePath FP.replaceExtension

dropExtension ::
  Applicative f =>
  ReadFilePathT f FilePath
dropExtension =
  liftReadFilePath FP.dropExtensions

addExtension ::
  Applicative f =>
  ReadFilePathT f (String -> FilePath)
addExtension =
  liftReadFilePath FP.addExtension

hasExtension ::
  Applicative f =>
  ReadFilePathT f Bool
hasExtension =
  liftReadFilePath FP.hasExtension

splitExtensions ::
  Applicative f =>
  ReadFilePathT f (FilePath, String)
splitExtensions =
  liftReadFilePath FP.splitExtensions

dropExtensions ::
  Applicative f =>
  ReadFilePathT f FilePath
dropExtensions =
  liftReadFilePath FP.dropExtensions

takeExtensions ::
  Applicative f =>
  ReadFilePathT f String
takeExtensions =
  liftReadFilePath FP.takeExtensions

replaceExtensions ::
  Applicative f =>
  ReadFilePathT f (String -> FilePath)
replaceExtensions =
  liftReadFilePath FP.replaceExtensions

isExtensionOf ::
  Applicative f =>
  String
  -> ReadFilePathT f Bool
isExtensionOf =
  liftReadFilePath . FP.isExtensionOf

stripExtension ::
  Applicative f =>
  String
  -> ReadFilePathT f (Maybe FilePath)
stripExtension =
  liftReadFilePath . FP.stripExtension

splitFileName ::
  Applicative f =>
  ReadFilePathT f (String, String)
splitFileName =
  liftReadFilePath FP.splitFileName

takeFileName ::
  Applicative f =>
  ReadFilePathT f String
takeFileName =
  liftReadFilePath FP.takeFileName

replaceFileName ::
  Applicative f =>
  ReadFilePathT f (String -> FilePath)
replaceFileName =
  liftReadFilePath FP.replaceFileName

dropFileName ::
  Applicative f =>
  ReadFilePathT f FilePath
dropFileName =
  liftReadFilePath FP.dropFileName

takeBaseName ::
  Applicative f =>
  ReadFilePathT f String
takeBaseName =
  liftReadFilePath FP.takeBaseName

replaceBaseName ::
  Applicative f =>
  ReadFilePathT f (String -> FilePath)
replaceBaseName =
  liftReadFilePath FP.replaceBaseName

takeDirectory ::
  Applicative f =>
  ReadFilePathT f FilePath
takeDirectory =
  liftReadFilePath FP.takeDirectory

replaceDirectory ::
  Applicative f =>
  ReadFilePathT f (String -> FilePath)
replaceDirectory =
  liftReadFilePath FP.replaceDirectory

combine ::
  Applicative f =>
  ReadFilePathT f (FilePath -> FilePath)
combine =
  liftReadFilePath FP.combine

splitPath ::
  Applicative f =>
  ReadFilePathT f [FilePath]
splitPath =
  liftReadFilePath FP.splitPath

joinPath ::
  ToFilePath [FilePath]
joinPath =
  toFilePath # FP.joinPath

splitDirectories ::
  Applicative f =>
  ReadFilePathT f [FilePath]
splitDirectories =
  liftReadFilePath FP.splitDirectories

splitDrive ::
  Applicative f =>
  ReadFilePathT f (FilePath, FilePath)
splitDrive =
  liftReadFilePath FP.splitDrive

joinDrive ::
  Applicative f =>
  ReadFilePathT f (FilePath -> FilePath)
joinDrive =
  liftReadFilePath FP.joinDrive

takeDrive ::
  Applicative f =>
  ReadFilePathT f FilePath
takeDrive =
  liftReadFilePath FP.takeDrive

hasDrive ::
  Applicative f =>
  ReadFilePathT f Bool
hasDrive =
  liftReadFilePath FP.hasDrive

dropDrive ::
  Applicative f =>
  ReadFilePathT f FilePath
dropDrive =
  liftReadFilePath FP.dropDrive

isDrive ::
  Applicative f =>
  ReadFilePathT f Bool
isDrive =
  liftReadFilePath FP.isDrive

hasTrailingPathSeparator ::
  Applicative f =>
  ReadFilePathT f Bool
hasTrailingPathSeparator =
  liftReadFilePath FP.hasTrailingPathSeparator

addTrailingPathSeparator ::
  Applicative f =>
  ReadFilePathT f FilePath
addTrailingPathSeparator =
  liftReadFilePath FP.addTrailingPathSeparator

dropTrailingPathSeparator ::
  Applicative f =>
  ReadFilePathT f FilePath
dropTrailingPathSeparator =
  liftReadFilePath FP.dropTrailingPathSeparator

normalise ::
  Applicative f =>
  ReadFilePathT f FilePath
normalise =
  liftReadFilePath FP.normalise

equalFilePath ::
  Applicative f =>
  ReadFilePathT f (FilePath -> Bool)
equalFilePath =
  liftReadFilePath FP.equalFilePath

makeRelative ::
  Applicative f =>
  ReadFilePathT f (FilePath -> FilePath)
makeRelative =
  liftReadFilePath FP.makeRelative

isRelative ::
  Applicative f =>
  ReadFilePathT f Bool
isRelative =
  liftReadFilePath FP.isRelative

isAbsolute ::
  Applicative f =>
  ReadFilePathT f Bool
isAbsolute =
  liftReadFilePath FP.isAbsolute

isValid ::
  Applicative f =>
  ReadFilePathT f Bool
isValid =
  liftReadFilePath FP.isValid

makeValid ::
  Applicative f =>
  ReadFilePathT f FilePath
makeValid =
  liftReadFilePath FP.makeValid
