{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module System.FilePath.FilePather.ToFilePath(
  ToFilePathT(..)
, ToFilePath
, toFilePath
, toRead
) where

import Control.Category ( Category((.)) )
import Control.Lens
    ( view,
      from,
      iso,
      Iso,
      Iso',
      Wrapped(..) )
import Data.Functor.Contravariant ( Contravariant(contramap) )
import Data.Functor.Identity ( Identity(..) )
import System.FilePath ( FilePath )
import System.FilePath.FilePather.ReadFilePath
    ( ReadFilePath, readFilePath )

newtype ToFilePathT f a =
  ToFilePathT (a -> f FilePath)

type ToFilePath a =
  ToFilePathT Identity a

instance Wrapped (ToFilePathT f a) where
  type Unwrapped (ToFilePathT f a) =
    a
    -> f FilePath
  _Wrapped' =
    iso (\(ToFilePathT x) -> x) ToFilePathT
  {-# INLINE _Wrapped' #-}

instance Contravariant (ToFilePathT f) where
  contramap f (ToFilePathT g) =
    ToFilePathT (g . f)

toFilePath ::
  Iso
    (ToFilePath a)
    (ToFilePath a')
    (a -> FilePath)
    (a' -> FilePath)
toFilePath =
  iso
    (\(ToFilePathT x) -> runIdentity . x)
    (\p -> ToFilePathT (Identity . p))
{-# INLINE toFilePath #-}

toRead ::
  Iso'
    (ToFilePath FilePath)
    (ReadFilePath FilePath)
toRead =
  iso
    (view (toFilePath . from readFilePath))
    (view (readFilePath . from toFilePath))
