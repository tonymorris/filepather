{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module System.FilePath.ReadFilePath(
  ReadFilePathT(..)
, ReadFilePath
, readFilePath
) where

import Control.Applicative
    ( Applicative(liftA2, (<*>), pure), Alternative((<|>), empty) )
import Control.Category((.))
import Control.Lens
    ( iso, Iso, Rewrapped, Wrapped(Unwrapped, _Wrapped'), view, _Wrapped )
import Control.Monad
    ( Monad((>>=), return), Functor(fmap), MonadPlus(mplus, mzero) )
import Control.Monad.Cont.Class ( MonadCont(callCC) )
import Control.Monad.Error.Class ( MonadError(throwError, catchError) )
import Control.Monad.Fail ( MonadFail(fail) )
import Control.Monad.Fix ( MonadFix(mfix) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Morph ( MFunctor(hoist), MMonad(embed) )
import Control.Monad.Reader.Class ( MonadReader(reader, local, ask) )
import Control.Monad.State.Class ( MonadState(state, get, put) )
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Writer.Class ( MonadWriter(pass, tell, writer, listen) )
import Control.Monad.Zip ( MonadZip(mzipWith) )
import Data.Functor.Apply ( Apply(liftF2, (<.>)) )
import Data.Functor.Alt ( Alt((<!>)) )
import Data.Functor.Bind ( Bind((>>-)) )
import Data.Functor.Identity(Identity(Identity, runIdentity))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Semigroup(Semigroup((<>)))
import System.FilePath(FilePath)

newtype ReadFilePathT f a =
  ReadFilePathT (FilePath -> f a)

instance ReadFilePathT f_a7Wv a_a7Ww ~ t_a7Wu =>
  Rewrapped (ReadFilePathT f_a51U a_a51V) t_a7Wu

instance Wrapped (ReadFilePathT f a) where
  type Unwrapped (ReadFilePathT f a) =
    FilePath
    -> f a
  _Wrapped' =
    iso (\(ReadFilePathT x) -> x) ReadFilePathT
  {-# INLINE _Wrapped' #-}

type ReadFilePath a =
  ReadFilePathT Identity a

readFilePath ::
  Iso
    (ReadFilePath a)
    (ReadFilePath a')
    (FilePath -> a)
    (FilePath -> a')
readFilePath =
  iso
    (\(ReadFilePathT x) -> runIdentity . x)
    (\p -> ReadFilePathT (Identity . p))
{-# INLINE readFilePath #-}

instance (Apply f, Semigroup a) => Semigroup (ReadFilePathT f a) where
  ReadFilePathT a <> ReadFilePathT b =
    ReadFilePathT (\p -> liftF2 (<>) (a p) (b p))
  {-# INLINE (<>) #-}

instance (Apply f, Applicative f, Monoid a) => Monoid (ReadFilePathT f a) where
  ReadFilePathT a `mappend` ReadFilePathT b =
    ReadFilePathT (\p -> liftA2 mappend (a p) (b p))
  {-# INLINE mappend #-}
  mempty =
    ReadFilePathT (pure (pure mempty))
  {-# INLINE mempty #-}

instance Functor f => Functor (ReadFilePathT f) where
  fmap f (ReadFilePathT x) =
    ReadFilePathT (fmap (fmap f) x)
  {-# INLINE fmap #-}

instance Apply f => Apply (ReadFilePathT f) where
  ReadFilePathT f <.> ReadFilePathT a =
    ReadFilePathT (\p -> f p <.> a p)
  {-# INLINE (<.>) #-}

instance Bind f => Bind (ReadFilePathT f) where
  ReadFilePathT f >>- g =
    ReadFilePathT (\p -> f p >>- \a -> view _Wrapped (g a) p)
  {-# INLINE (>>-) #-}

instance Applicative f => Applicative (ReadFilePathT f) where
  ReadFilePathT f <*> ReadFilePathT a =
    ReadFilePathT (\p -> f p <*> a p)
  {-# INLINE (<*>) #-}
  pure =
    ReadFilePathT . pure . pure
  {-# INLINE pure #-}

instance Alt f => Alt (ReadFilePathT f) where
  ReadFilePathT a <!> ReadFilePathT b =
    ReadFilePathT (\p -> a p <!> b p)
  {-# INLINE (<!>) #-}

instance Alternative f => Alternative (ReadFilePathT f) where
  ReadFilePathT a <|> ReadFilePathT b =
    ReadFilePathT (\p -> a p <|> b p)
  {-# INLINE (<|>) #-}
  empty =
    ReadFilePathT (pure empty)
  {-# INLINE empty #-}

instance Monad f => Monad (ReadFilePathT f) where
  ReadFilePathT f >>= g =
    ReadFilePathT (\p -> f p >>= \a -> view _Wrapped (g a) p)
  {-# INLINE (>>=) #-}
  return =
    ReadFilePathT . return . return
  {-# INLINE return #-}

instance MonadTrans ReadFilePathT where
  lift =
    ReadFilePathT . pure
  {-# INLINE lift #-}

instance MonadIO f => MonadIO (ReadFilePathT f) where
  liftIO =
    ReadFilePathT . pure . liftIO
  {-# INLINE liftIO #-}

instance MFunctor ReadFilePathT where
  hoist k (ReadFilePathT f) =
    ReadFilePathT (k .f)
  {-# INLINE hoist #-}

instance MMonad ReadFilePathT where
  embed k (ReadFilePathT f) =
    ReadFilePathT (\p -> view _Wrapped (k (f p)) p)
  {-# INLINE embed #-}

instance Monad f => MonadReader FilePath (ReadFilePathT f) where
  ask =
    ReadFilePathT pure
  {-# INLINE ask #-}
  local k (ReadFilePathT f) =
    ReadFilePathT (f . k)
  {-# INLINE local #-}
  reader k =
    ReadFilePathT (pure . k)
  {-# INLINE reader #-}

instance MonadState FilePath f => MonadState FilePath (ReadFilePathT f) where
  state =
    lift . state
  {-# INLINE state #-}
  get =
    lift get
  {-# INLINE get #-}
  put =
    lift . put
  {-# INLINE put #-}

instance MonadWriter FilePath f => MonadWriter FilePath (ReadFilePathT f) where
  writer =
    lift . writer
  {-# INLINE writer #-}
  tell =
    lift . tell
  {-# INLINE tell #-}
  listen (ReadFilePathT f) =
    ReadFilePathT (listen . f)
  {-# INLINE listen #-}
  pass (ReadFilePathT f) =
    ReadFilePathT (pass . f)
  {-# INLINE pass #-}

instance MonadFail f => MonadFail (ReadFilePathT f) where
  fail =
    lift . fail
  {-# INLINE fail #-}

instance MonadFix f => MonadFix (ReadFilePathT f) where
  mfix f =
    ReadFilePathT (\p -> mfix (\a -> view _Wrapped (f a) p))
  {-# INLINE mfix #-}

instance MonadZip f => MonadZip (ReadFilePathT f) where
  mzipWith f (ReadFilePathT m) (ReadFilePathT n) =
    ReadFilePathT (\a -> mzipWith f (m a) (n a))
  {-# INLINE mzipWith #-}

instance MonadCont f => MonadCont (ReadFilePathT f) where
  callCC p =
    ReadFilePathT (\r -> callCC (\c -> view _Wrapped (p (ReadFilePathT . pure . c)) r))
  {-# INLINE callCC #-}

instance MonadError e f => MonadError e (ReadFilePathT f) where
  throwError =
    lift . throwError
  {-# INLINE throwError #-}
  catchError (ReadFilePathT f) g =
    ReadFilePathT (\ r -> catchError (f r) (\ e -> view _Wrapped (g e) r))
  {-# INLINE catchError #-}

instance (MonadPlus f) => MonadPlus (ReadFilePathT f) where
  mzero =
    lift mzero
  {-# INLINE mzero #-}
  ReadFilePathT a `mplus` ReadFilePathT b =
    ReadFilePathT (\ r -> a r `mplus` b r)
  {-# INLINE mplus #-}
