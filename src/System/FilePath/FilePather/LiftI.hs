{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module System.FilePath.FilePather.LiftI
(
  LiftI(..)
) where

import System.FilePath.FilePather.RecursePredicate
import System.FilePath.FilePather.FilterPredicate

-- | A type-class for lifting a value.
-- This type-class probably belongs elsewhere (pointers appreciated!).
class LiftI f a | f -> a where
  liftI ::
    Monad g =>
    g a
    -> f g

instance LiftI RecursePredicateT Bool where
  liftI = 
    recursePredicateT . const

instance LiftI FilterPredicateT Bool where
  liftI = 
    filterPredicateT . const . const

