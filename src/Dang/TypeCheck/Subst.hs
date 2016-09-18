{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Dang.TypeCheck.Subst (
    Subst(), emptySubst,
    Zonk(), zonk,
    Unify(), unify,
  ) where

import Dang.ModuleSystem.Name (Name)
import Dang.Monad
import Dang.TypeCheck.AST (TVar,Type(..))
import Dang.Utils.PP

import           Control.Monad (mzero)
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import           GHC.Generics
import           MonadLib (runStateT,StateT,get,set,inBase)


-- Environment -----------------------------------------------------------------

data Subst = Subst { suCanon :: !(Map.Map TVar Int)
                     -- ^ Canonical names for unification variables -- unifying
                     -- two variables corresponds to manipulating this map only.

                   , suEnv :: !(IM.IntMap Type)
                     -- ^ Bindings to canonical names

                   , suNext :: !Int
                     -- ^ The next canonical name available.
                   }

emptySubst :: Subst
emptySubst  = Subst Map.empty IM.empty 0

-- | Merge two variables in the substitution environment.
merge :: TVar -> TVar -> Subst -> Maybe Subst
merge a b Subst { .. } =
  case (Map.lookup a suCanon, Map.lookup b suCanon) of

    (Just{}, Just{}) -> Nothing

    (Just x, Nothing) ->
      Just Subst { suCanon = Map.insert b x suCanon, .. }

    (Nothing, Just x) ->
      Just Subst { suCanon = Map.insert a x suCanon, .. }

    (Nothing,Nothing) ->
      Just Subst { suCanon = Map.insert a suNext
                           $ Map.insert b suNext suCanon
                 , suNext  = suNext + 1
                 , .. }


-- | Insert a type into the environment.
insertType :: TVar -> Type -> Subst -> Subst
insertType a ty Subst { .. } =
  case Map.lookup a suCanon of
    Just ix ->
      Subst { suEnv = IM.insert ix ty suEnv, .. }

    Nothing ->
      Subst { suCanon = Map.insert a suNext suCanon
            , suEnv   = IM.insert suNext ty suEnv
            , suNext  = suNext + 1 }


-- Monad -----------------------------------------------------------------------

type M = StateT Subst Dang

-- | The two types failed to unify.
unificationFailed :: Type -> Type -> M a
unificationFailed expected found =
  do addError ErrUnification msg
     mzero
  where
  msg = vcat [ hang (text "Expected type:") 2 (pp expected)
             , hang (text "   Found type:") 2 (pp found) ]

occursCheckFailed :: TVar -> Type -> M a
occursCheckFailed var ty =
  do addError ErrInfiniteType (undefined :: Doc)
     mzero


-- Zonking ---------------------------------------------------------------------

-- | Remove type variables from a type.
zonk :: (Zonk a, DangM m) => Subst -> a -> m (a,Subst)
zonk su a = inBase (runStateT su (zonk' Set.empty a))

class Zonk a where
  zonk' :: Set.Set TVar -> a -> M a

  default zonk' :: (Generic a, GZonk (Rep a)) => Set.Set TVar -> a -> M a
  zonk' seen a = to `fmap` gzonk' seen (from a)

instance Zonk () where
  zonk' _ () = return ()

instance Zonk a => Zonk [a]

instance Zonk Type where
  zonk' seen ty@(TFree v) =
    do Subst { .. } <- get
       case Map.lookup v suCanon of
         Just i ->
           case IM.lookup i suEnv of
             Just ty' | v `Set.member` seen -> occursCheckFailed v ty'
                      | otherwise           -> zonk' (Set.insert v seen) ty'

             Nothing -> return ty

         Nothing -> return ty

  zonk' seen ty@TGen{} =
    return ty

  zonk' seen ty@TCon{} =
    return ty

  zonk' seen (TApp f x) =
    do f' <- zonk' seen f
       x' <- zonk' seen x
       return (TApp f' x')

  zonk' seen (TFun a b) =
    do a' <- zonk' seen a
       b' <- zonk' seen b
       return (TApp a' b')



class GZonk (f :: * -> *) where
  gzonk' :: Set.Set TVar -> f a -> M (f a)

instance GZonk U1 where
  gzonk' _ u = return u

instance Zonk a => GZonk (K1 i a) where
  gzonk' seen (K1 a) = K1 `fmap` zonk' seen a

instance GZonk f => GZonk (M1 i c f) where
  gzonk' seen (M1 f) = M1 `fmap` gzonk' seen f

instance (GZonk f, GZonk g) => GZonk (f :+: g) where
  gzonk' seen (L1 f) = L1 `fmap` gzonk' seen f
  gzonk' seen (R1 g) = R1 `fmap` gzonk' seen g

instance (GZonk f, GZonk g) => GZonk (f :*: g) where
  gzonk' seen (f :*: g) =
    do f' <- gzonk' seen f
       g' <- gzonk' seen g
       return (f' :*: g')


-- Unification -----------------------------------------------------------------

unify :: (Unify a, DangM m) => Subst -> a -> a -> m Subst
unify su a b = inBase (snd `fmap` runStateT su (unify' a b))

class Zonk a => Unify a where
  unify' :: a -> a -> M ()

bindVar :: TVar -> Type -> M ()
bindVar var ty
    -- trivial case of a unification variable unifying with itself
  | TFree var == ty = return ()

    -- XXX should do kind checking as well

    -- merge variables
  | TFree var' <- ty =
    do su <- get
       case merge var var' su of
         Just su' -> set $! su'
         Nothing  -> unificationFailed (TFree var) ty

    -- allocate a fresh canonical name, and insert into the environment
  | otherwise =
    do su <- get
       set $! insertType var ty su
