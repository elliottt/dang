{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.TypeCheck.Monad (
    TC(), runTC,
    Subst.Unify, unify,
    Subst.Zonk, zonk, ftvs,
  ) where

import           Dang.Monad (Dang)
import           Dang.TypeCheck.AST (TVar)
import qualified Dang.TypeCheck.Subst as Subst

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import qualified Data.Set as Set
import           MonadLib (BaseM(..),StateT,get,set,runM)

data RW = RW { rwSubst :: !Subst.Subst
             }

newtype TC a = TC { unTC :: StateT RW Dang a
                  } deriving (Functor,Applicative,Monad)

runTC :: TC a -> Dang a
runTC (TC m) = fst `fmap` runM m RW { rwSubst = Subst.emptySubst }

instance BaseM TC Dang where
  inBase m = TC (inBase m)
  {-# INLINE inBase #-}

instance Alternative TC where
  empty   = TC empty
  a <|> b = TC (unTC a <|> unTC b)
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

instance MonadPlus TC where
  mzero     = TC mzero
  mplus a b = TC (unTC a `mplus` unTC b)
  {-# INLINE mzero #-}
  {-# INLINE mplus #-}

-- | Unify two things that have types, and update the internal state.
unify :: Subst.Unify a => a -> a -> TC ()
unify a b = TC $
  do RW { .. } <- get
     su' <- Subst.unify rwSubst a b
     set $! RW { rwSubst = su' }

-- | Remove type variables from a thing that has types.
--
-- NOTE: this will fail if the type given is infinite.
zonk :: Subst.Zonk a => a -> TC a
zonk a = TC $
  do RW { .. } <- get
     Subst.zonk rwSubst a

-- | Calculate the free variables of a thing that has types.
--
-- NOTE: this will fail with if the type given is infinite.
ftvs :: Subst.Zonk a => a -> TC (Set.Set TVar)
ftvs a = TC $
  do RW { .. } <- get
     Subst.ftvs rwSubst a
