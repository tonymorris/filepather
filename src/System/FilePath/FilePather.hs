module System.FilePath.FilePather where

import Control.Applicative(Applicative(pure, (<*>)), liftA2)
import Control.Category(Category((.)))
import Control.Monad(Monad((>>=), return), liftM)
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)), liftF2)
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Bind.Trans(BindTrans(liftB))
import System.FilePath(FilePath)

newtype FilePathState f a =
  FilePathState (FilePath -> f (FilePath, a))

instance Functor f => Functor (FilePathState f) where
  fmap f (FilePathState k) =
    FilePathState (fmap (fmap f) . k)

instance Bind f => Apply (FilePathState f) where
  FilePathState f <.> FilePathState a =
    FilePathState (\p -> f p >>- \(q, g) -> fmap (fmap g) (a q))

instance (Bind f, Applicative f) => Applicative (FilePathState f) where
  FilePathState f <*> FilePathState a =
    FilePathState (\p -> f p >>- \(q, g) -> fmap (fmap g) (a q))
  pure a =
    FilePathState (\p -> pure (p, a))

instance Bind f => Bind (FilePathState f) where
  FilePathState k >>- f =
    FilePathState (\p -> k p >>- \(q, a) -> let FilePathState s = f a in s q)

instance Monad f => Monad (FilePathState f) where
  FilePathState k >>= f =
    FilePathState (\p -> k p >>= \(q, a) -> let FilePathState s = f a in s q)
  return a =
    FilePathState (\p -> return (p, a))

instance MonadTrans FilePathState where
  lift a =
    FilePathState (\p -> liftM ((,) p) a)

instance BindTrans FilePathState where
  liftB a =
    FilePathState (\p -> fmap ((,) p) a)

instance Applicative f => GetFilePath (FilePathState f) where
  getFilePath =
    FilePathState (\p -> pure (p, p))

instance Applicative f => PutFilePath (FilePathState f) where
  putFilePath p =
    FilePathState (\_ -> pure (p, ()))

----

newtype FilePathReader f a =
  FilePathReader (FilePath -> f a)

instance Functor f => Functor (FilePathReader f) where
  fmap f (FilePathReader k) =
    FilePathReader (fmap f . k)

instance Apply f => Apply (FilePathReader f) where
  FilePathReader f <.> FilePathReader a =
    FilePathReader (liftF2 (<.>) f a)
  
instance Applicative f => Applicative (FilePathReader f) where
  FilePathReader f <*> FilePathReader a =
    FilePathReader (liftA2 (<*>) f a)
  pure =
    FilePathReader . pure . pure

instance Bind f => Bind (FilePathReader f) where
  FilePathReader k >>- f =
    FilePathReader (\p -> k p >>- \a -> let FilePathReader r = f a in r p)

instance Monad f => Monad (FilePathReader f) where
  FilePathReader k >>= f =
    FilePathReader (\p -> k p >>= \a -> let FilePathReader r = f a in r p)
  return =
    FilePathReader . return . return

instance MonadTrans FilePathReader where
  lift =
    FilePathReader . pure

instance BindTrans FilePathReader where
  liftB =
    FilePathReader . pure

instance Applicative f => GetFilePath (FilePathReader f) where
  getFilePath =
    FilePathReader pure

----

readState ::
  Functor f =>
  FilePathReader f a
  -> FilePathState f a
readState (FilePathReader k) =
  FilePathState (\p -> fmap (\a -> (p, a)) (k p))

----

class Functor f => GetFilePath f where
  getFilePath ::
    f FilePath

----

class Functor f => PutFilePath f where
  putFilePath ::
    FilePath
    -> f ()
