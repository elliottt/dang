{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.TypeCheck.Subst (
    Subst(), emptySubst, modifySkolems,
    Zonk(), zonk, ftvs,
    Unify(), unify,
  ) where

import Dang.ModuleSystem.Name (mkBinding,mkParam,ParamSource(..))
import Dang.Monad
import Dang.TypeCheck.AST (TVar(..),Type(..))
import Dang.Utils.PP
import Dang.Unique (withSupply)
import Dang.Syntax.Format (formatMessage)
import Dang.Syntax.Location (Source(..),interactive,emptyRange)

import           Control.Monad (mzero,unless)
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

                   , suSkolems :: !(Set.Set TVar)
                     -- ^ The set of Skolemized variables
                   }

emptySubst :: Subst
emptySubst  = Subst Map.empty IM.empty 0 Set.empty

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
            , suNext  = suNext + 1
            , .. }


-- | Modify the set of Skolem variables
modifySkolems :: (Set.Set TVar -> Set.Set TVar) -> (Subst -> Subst)
modifySkolems f Subst { .. } = Subst { suSkolems = f suSkolems, .. }


-- Monad -----------------------------------------------------------------------

type M = StateT Subst Dang

-- | Lookup the binding for a type variable, if it exists.
lookupType :: TVar -> M (Maybe Type)
lookupType var =
  do Subst { .. } <- get
     case Map.lookup var suCanon of
       Just i  -> return (IM.lookup i suEnv)
       Nothing -> return Nothing

-- | The two types failed to unify.
unificationFailed :: (PP a, PP b) => a -> b -> M r
unificationFailed expected found =
  do addError ErrUnification $
       vcat [ hang (text "Expected type:") 2 (pp expected)
            , hang (text "   Found type:") 2 (pp found) ]
     mzero

occursCheckFailed :: TVar -> Type -> M a
occursCheckFailed var ty =
  do addError ErrInfiniteType $
       hang (text "Cannot construct the infinite type:")
          2 (pp (TFree var) <+> char '~' <+> pp ty)
     mzero


-- Zonking ---------------------------------------------------------------------

-- | Remove type variables from a type.
zonk :: (Zonk a, DangM m) => Subst -> a -> m a
zonk su a = inBase (fst `fmap` runStateT su (zonk' Set.empty a))

ftvs :: (Zonk a, DangM m) => Subst -> a -> m (Set.Set TVar)
ftvs su a = inBase (fst `fmap` runStateT su (ftvs' Set.empty a))

class Zonk a where
  zonk' :: Set.Set TVar -> a -> M a
  ftvs' :: Set.Set TVar -> a -> M (Set.Set TVar)

  default zonk' :: (Generic a, GZonk (Rep a)) => Set.Set TVar -> a -> M a
  zonk' seen a = to `fmap` gzonk' seen (from a)

  default ftvs' :: (Generic a, GZonk (Rep a)) => Set.Set TVar -> a -> M (Set.Set TVar)
  ftvs' seen a = gftvs' seen (from a)


instance Zonk ()
instance Zonk a => Zonk (Maybe a)
instance Zonk a => Zonk [a]


resolve :: Set.Set TVar -> TVar -> M (Maybe (Set.Set TVar,Type))
resolve seen v =
  do Subst { .. } <- get
     case Map.lookup v suCanon of
       Just i ->
         case IM.lookup i suEnv of
           Just ty' | v `Set.member` seen -> occursCheckFailed v ty'
                    | otherwise           -> return (Just (Set.insert v seen,ty'))

           Nothing -> return Nothing

       Nothing -> return Nothing

instance Zonk Type where
  zonk' seen ty@(TFree v) =
    do mb <- resolve seen v
       case mb of
         Just (seen',ty') -> zonk' seen' ty'
         Nothing          -> return ty

  zonk' _ ty@TGen{} =
    return ty

  zonk' _ ty@TCon{} =
    return ty

  zonk' seen (TApp f x) =
    do f' <- zonk' seen f
       x' <- zonk' seen x
       return (TApp f' x')

  zonk' seen (TFun a b) =
    do a' <- zonk' seen a
       b' <- zonk' seen b
       return (TFun a' b')


  ftvs' seen (TFree v) =
    do mb <- resolve seen v
       case mb of
         Just (seen', ty') -> ftvs' seen' ty'
         Nothing           -> return Set.empty

  ftvs' _ TGen{} =
    return Set.empty

  ftvs' _ TCon{} =
    return Set.empty

  ftvs' seen (TApp a b) =
    do as <- ftvs' seen a
       bs <- ftvs' seen b
       return (as `Set.union` bs)

  ftvs' seen (TFun a b) =
    do as <- ftvs' seen a
       bs <- ftvs' seen b
       return (as `Set.union` bs)


class GZonk (f :: * -> *) where
  gzonk' :: Set.Set TVar -> f a -> M (f a)
  gftvs' :: Set.Set TVar -> f a -> M (Set.Set TVar)

instance GZonk U1 where
  gzonk' _ u = return u
  gftvs' _ _ = return Set.empty

instance Zonk a => GZonk (K1 i a) where
  gzonk' seen (K1 a) = K1 `fmap` zonk' seen a
  gftvs' seen (K1 a) = ftvs' seen a

instance GZonk f => GZonk (M1 i c f) where
  gzonk' seen (M1 f) = M1 `fmap` gzonk' seen f
  gftvs' seen (M1 f) = gftvs' seen f

instance (GZonk f, GZonk g) => GZonk (f :+: g) where
  gzonk' seen (L1 f) = L1 `fmap` gzonk' seen f
  gzonk' seen (R1 g) = R1 `fmap` gzonk' seen g

  gftvs' seen (L1 f) = gftvs' seen f
  gftvs' seen (R1 g) = gftvs' seen g

instance (GZonk f, GZonk g) => GZonk (f :*: g) where
  gzonk' seen (f :*: g) =
    do f' <- gzonk' seen f
       g' <- gzonk' seen g
       return (f' :*: g')

  gftvs' seen (f :*: g) =
    do fs <- gftvs' seen f
       gs <- gftvs' seen g
       return (fs `Set.union` gs)


-- Unification -----------------------------------------------------------------

unify :: (Unify a, DangM m) => Subst -> a -> a -> m Subst
unify su a b = inBase (snd `fmap` runStateT su (unify' a b))

class (PP a, Zonk a) => Unify a where
  unify' :: a -> a -> M ()

  default unify' :: (Generic a, GUnify (Rep a)) => a -> a -> M ()
  unify' a b =
    do success <- gunify' (from a) (from b)
       unless success (unificationFailed a b)

instance (PP a, Unify a) => Unify (Maybe a)
instance (PP a, Unify a) => Unify [a]

instance Unify Type where
  unify' (TFree a) ty =
    do mb <- lookupType a
       case mb of
         Just ty' -> unify' ty' ty
         Nothing  -> bindVar a ty

  unify' ty (TFree a) =
    do mb <- lookupType a
       case mb of
         Just ty' -> unify' ty ty'
         Nothing  -> bindVar a ty

  unify' (TCon a) (TCon b) | a == b = return ()

  unify' (TGen a) (TGen b) | a == b = return ()

  unify' (TApp a b) (TApp x y) =
    do unify' a x
       unify' b y

  unify' (TFun a b) (TFun x y) =
    do unify' a x
       unify' b y

  unify' a b = unificationFailed a b

class GZonk f => GUnify f where
  gunify' :: f a -> f b -> M Bool

instance GUnify U1 where
  gunify' U1 U1 = return True

instance Unify a => GUnify (K1 i a) where
  gunify' (K1 a) (K1 b) =
    do unify' a b
       return True

instance GUnify f => GUnify (M1 i c f) where
  gunify' (M1 a) (M1 b) = gunify' a b

instance (GUnify f, GUnify g) => GUnify (f :+: g) where
  gunify' (L1 a) (L1 b) = gunify' a b
  gunify' (R1 a) (R1 b) = gunify' a b
  gunify' _      _      = return False

instance (GUnify f, GUnify g) => GUnify (f :*: g) where
  gunify' (x :*: y) (a :*: b) =
    do r <- gunify' x a
       if r then gunify' y b
            else return r

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


test = runDang $
  do cxt  <- withSupply (mkBinding "Main" "cxt" emptyRange)
     fooC <- withSupply (mkBinding "Main" "Foo" emptyRange)
     a    <- withSupply (mkParam (FromBind cxt) "a" emptyRange)
     b    <- withSupply (mkParam (FromBind cxt) "b" emptyRange)
     su   <- unify emptySubst (TFree (TVar a)) (TCon fooC)
     su'  <- unify su (TFree (TVar a)) (TFree (TVar b))

     fun <- zonk su' (TFun (TFree (TVar a)) (TFree (TVar b)))
     io (print (pp fun))

     c   <- withSupply (mkParam (FromBind cxt) "c" emptyRange)
     let var = TFree (TVar c)
     su'' <- unify su' var (TFun var var)

     (c',ms) <- collectMessages (try (zonk su'' var))

     io (mapM_ (print . formatMessage interactive "") ms)
     io (print c')

     return ()
