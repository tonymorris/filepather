module System.FilePath.FilePather where

import Control.Applicative(Applicative(pure, (<*>)), liftA2)
import Control.Category(Category((.)))
import Control.Lens.Iso(Iso, iso)
import Control.Monad(Monad((>>=), return), liftM)
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Bool(Bool)
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)), liftF2)
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Bind.Trans(BindTrans(liftB))
import Data.Functor.Identity(Identity(Identity, runIdentity))
import Data.String(String)
import System.FilePath(FilePath)
import qualified System.FilePath as SP

newtype FilePathStateT f a =
  FilePathStateT (FilePath -> f (FilePath, a))

instance Functor f => Functor (FilePathStateT f) where
  fmap f (FilePathStateT k) =
    FilePathStateT (fmap (fmap f) . k)

instance Bind f => Apply (FilePathStateT f) where
  FilePathStateT f <.> FilePathStateT a =
    FilePathStateT (\p -> f p >>- \(q, g) -> fmap (fmap g) (a q))

instance (Bind f, Applicative f) => Applicative (FilePathStateT f) where
  FilePathStateT f <*> FilePathStateT a =
    FilePathStateT (\p -> f p >>- \(q, g) -> fmap (fmap g) (a q))
  pure a =
    FilePathStateT (\p -> pure (p, a))

instance Bind f => Bind (FilePathStateT f) where
  FilePathStateT k >>- f =
    FilePathStateT (\p -> k p >>- \(q, a) -> let FilePathStateT s = f a in s q)

instance Monad f => Monad (FilePathStateT f) where
  FilePathStateT k >>= f =
    FilePathStateT (\p -> k p >>= \(q, a) -> let FilePathStateT s = f a in s q)
  return a =
    FilePathStateT (\p -> return (p, a))

instance MonadTrans FilePathStateT where
  lift a =
    FilePathStateT (\p -> liftM ((,) p) a)

instance BindTrans FilePathStateT where
  liftB a =
    FilePathStateT (\p -> fmap ((,) p) a)

instance Applicative f => GetFilePath (FilePathStateT f) where
  getFilePath =
    FilePathStateT (\p -> pure (p, p))

instance Applicative f => PutFilePath (FilePathStateT f) where
  putFilePath p =
    FilePathStateT (\_ -> pure (p, ()))
  modifyFilePath k =
    FilePathStateT (\p -> pure (k p, ()))

type FilePathState a =
  FilePathStateT Identity a

filePathStateTIso ::
  Iso (FilePathStateT f a) (FilePathStateT f b) (FilePath -> f (FilePath, a)) (FilePath -> f (FilePath, b))
filePathStateTIso =
  iso (\(FilePathStateT k) -> k) FilePathStateT

filePathStateIso ::
  Iso (FilePathState a) (FilePathState b) (FilePath -> (FilePath, a)) (FilePath -> (FilePath, b))
filePathStateIso =
  iso (\(FilePathStateT k) -> runIdentity . k) (\k -> FilePathStateT (Identity . k))

----

newtype FilePathReaderT f a =
  FilePathReaderT (FilePath -> f a)

instance Functor f => Functor (FilePathReaderT f) where
  fmap f (FilePathReaderT k) =
    FilePathReaderT (fmap f . k)

instance Apply f => Apply (FilePathReaderT f) where
  FilePathReaderT f <.> FilePathReaderT a =
    FilePathReaderT (liftF2 (<.>) f a)
  
instance Applicative f => Applicative (FilePathReaderT f) where
  FilePathReaderT f <*> FilePathReaderT a =
    FilePathReaderT (liftA2 (<*>) f a)
  pure =
    FilePathReaderT . pure . pure

instance Bind f => Bind (FilePathReaderT f) where
  FilePathReaderT k >>- f =
    FilePathReaderT (\p -> k p >>- \a -> let FilePathReaderT r = f a in r p)

instance Monad f => Monad (FilePathReaderT f) where
  FilePathReaderT k >>= f =
    FilePathReaderT (\p -> k p >>= \a -> let FilePathReaderT r = f a in r p)
  return =
    FilePathReaderT . return . return

instance MonadTrans FilePathReaderT where
  lift =
    FilePathReaderT . pure

instance BindTrans FilePathReaderT where
  liftB =
    FilePathReaderT . pure

instance Applicative f => GetFilePath (FilePathReaderT f) where
  getFilePath =
    FilePathReaderT pure

type FilePathReader a =
  FilePathReaderT Identity a

filePathReaderTIso ::
  Iso (FilePathReaderT f a) (FilePathReaderT f b) (FilePath -> f a) (FilePath -> f b)
filePathReaderTIso =
  iso (\(FilePathReaderT k) -> k) FilePathReaderT

filePathReaderIso ::
  Iso (FilePathReader a) (FilePathReader b) (FilePath -> a) (FilePath -> b)
filePathReaderIso =
  iso (\(FilePathReaderT k) -> runIdentity . k) (\k -> FilePathReaderT (Identity . k))

----

readState ::
  Functor f =>
  FilePathReaderT f a
  -> FilePathStateT f a
readState (FilePathReaderT k) =
  FilePathStateT (\p -> fmap (\a -> (p, a)) (k p))

----

class Functor f => GetFilePath f where
  getFilePath ::
    f FilePath

----

class GetFilePath f => PutFilePath f where
  putFilePath ::
    FilePath
    -> f ()
  modifyFilePath ::
    (FilePath -> FilePath)
    -> f ()    

----

splitExtension ::
  GetFilePath f =>
  f (String, String)
splitExtension =
  fmap SP.splitExtension getFilePath 

takeExtension ::
  GetFilePath f =>
  f String
takeExtension =
  fmap SP.takeExtension getFilePath   

replaceExtension ::
  PutFilePath f =>
  String
  -> f ()
replaceExtension ext = 
  modifyFilePath (`SP.replaceExtension` ext)

dropExtension ::
  PutFilePath f =>
  f ()
dropExtension =
  modifyFilePath SP.dropExtension

addExtension ::
  PutFilePath f =>
  String
  -> f ()
addExtension ext = 
  modifyFilePath (`SP.addExtension` ext)

hasExtension ::
  GetFilePath f =>
  f Bool
hasExtension =
  fmap SP.hasExtension getFilePath 

splitExtensions ::
  GetFilePath f =>
  f (FilePath, String)
splitExtensions =
  fmap SP.splitExtensions getFilePath 

dropExtensions ::
  PutFilePath f =>
  f ()
dropExtensions =
  modifyFilePath SP.dropExtensions

takeExtensions ::
  GetFilePath f =>
  f String
takeExtensions =
  fmap SP.takeExtensions getFilePath 

splitDrive ::
  GetFilePath f =>
  f (FilePath, FilePath)
splitDrive =
  fmap SP.splitDrive getFilePath 

joinDrive ::
  (GetFilePath f, PutFilePath g) =>
  f (g ())
joinDrive =
  fmap (modifyFilePath . SP.joinDrive) getFilePath

takeDrive ::
  PutFilePath f =>
  f ()
takeDrive =
  modifyFilePath SP.takeDrive
  
hasDrive ::
  GetFilePath f =>
  f Bool
hasDrive =
  fmap SP.hasDrive getFilePath 

dropDrive ::
  PutFilePath f =>
  f ()
dropDrive =
  modifyFilePath SP.dropDrive
  
isDrive ::
  GetFilePath f =>
  f Bool
isDrive =
  fmap SP.isDrive getFilePath 
