{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeChecker.Monad where

import Dang.Monad
import TypeChecker.Types
import TypeChecker.Unify

import Control.Monad.Fix (MonadFix)
import MonadLib
import qualified Data.Map as Map


newtype TC a = TC { unTC :: StateT RW Dang a }
    deriving (Functor,Monad,MonadFix)

instance BaseM TC Dang where
  inBase = TC . inBase

instance ExceptionM TC SomeException where
  raise = TC . raise

instance RunExceptionM TC SomeException where
  try = TC . try . unTC

data RW = RW
  { rwSubst :: Subst
  , rwIndex :: !Int
  }

data RO = RO
  { roEnv :: Map.Map String Type
  }


-- Primitive Operations --------------------------------------------------------

-- | Unify two types, modifying the internal substitution.
unify :: Type -> Type -> TC ()
unify l r = do
  u <- getSubst
  extSubst =<< mgu (apply u l) (apply u r)

-- | Get the current substitution.
getSubst :: TC Subst
getSubst  = rwSubst `fmap` TC get

-- | Extend the current substitution by merging in the given one.
extSubst :: Subst -> TC ()
extSubst u = do
  rw <- TC get
  TC (set rw { rwSubst = u @@ rwSubst rw })

-- | Apply the substitution to a type.
applySubst :: Type -> TC Type
applySubst ty = do
  rw <- TC get
  return (apply (rwSubst rw) ty)

-- | Generate fresh type variables, with the given kind.
freshVar :: Kind -> TC Type
freshVar k = do
  rw <- TC get
  TC (set rw { rwIndex = rwIndex rw + 1 })
  return (TVar (rwIndex rw) (TParam "" k))
