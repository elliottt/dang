{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.Unify where

import Dang.Core.AST
import Dang.ModuleSystem.QualName ( QualName )
import Dang.Monad
import Dang.Syntax.AST (DataDecl(..),ConstrGroup(..),Constr(..))
import Dang.TypeChecker.Types
import Dang.Utils.Pretty

import Control.Arrow (second)
import Control.Monad ( unless, guard, mzero )
import Data.List (intersect)
import qualified Data.Set as Set
import qualified Data.Map as Map


-- Substitution ----------------------------------------------------------------

data Subst = Subst
  { substUnbound :: Map.Map Index Type
  , substBound   :: Map.Map Index Type
  } deriving (Show)

-- | Lookup a variable index in a substitution.
lookupGen :: Index -> Subst -> Maybe Type
lookupGen i = Map.lookup i . substBound

-- | Lookup a variable index in a substitution.
lookupVar :: Index -> Subst -> Maybe Type
lookupVar i = Map.lookup i . substUnbound

-- | The empty substitution.
emptySubst :: Subst
emptySubst  = Subst
  { substUnbound = Map.empty
  , substBound   = Map.empty
  }

-- | Generate a singleton generic substitution.
genSubst :: Index -> Type -> Subst
genSubst v ty = emptySubst { substBound = Map.singleton v ty }

-- | Generate a singleton variable substitution.
varSubst :: Index -> Type -> Subst
varSubst v ty = emptySubst { substUnbound = Map.singleton v ty }

-- | Generate a substitution on unbound variables.
unboundSubst :: [(Index,Type)] -> Subst
unboundSubst u = emptySubst { substUnbound = Map.fromList u }

-- | Generate a substitution on bound variables.
boundSubst :: [(Index,Type)] -> Subst
boundSubst u = emptySubst { substBound = Map.fromList u }

-- | Compose two substitutions.
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = Subst
  { substBound = Map.map (apply s1) (substBound s2)
      `Map.union` substBound s1
  , substUnbound = Map.map (apply s1) (substUnbound s2)
      `Map.union` substUnbound s1
  }


-- Type Interface --------------------------------------------------------------

apply :: Types a => Subst -> a -> a
apply  = apply' 0


class Types a where
  apply'   :: Int -> Subst -> a -> a
  typeVars :: a -> Set.Set (TParam Kind)

instance Types a => Types [a] where
  apply' b u = map (apply' b u)
  typeVars   = Set.unions . map typeVars

instance (Ord a, Types a) => Types (Set.Set a) where
  apply' b u = Set.map (apply' b u)
  typeVars   = Set.unions . Set.toList . Set.map typeVars

instance Types Type where
  apply' b u ty = case ty of
    TApp f x     -> TApp (apply' b u f) (apply' b u x)
    TInfix n l r -> TInfix n (apply' b u l) (apply' b u r)
    TVar p       -> apply'TVar b u p
    TCon{}       -> ty

  typeVars ty = case ty of
    TApp f x     -> typeVars f `Set.union` typeVars x
    TInfix _ l r -> typeVars l `Set.union` typeVars r
    TVar tv      -> typeVarsTVar tv
    TCon{}       -> Set.empty

apply'TVar :: Int -> Subst -> TVar Kind -> Type
apply'TVar b u tv = case tv of

  -- when an unbound variable is found, apply the substitution without adjusting
  -- the parameter index.
  UVar p -> process (lookupVar (paramIndex p) u)

  -- when a bound variable is found, make sure that it is reachable from a
  -- quantifier outside any that may have been crossed, and then adjust its
  -- index to be the same as the outer most quantifier.
  GVar p -> process $ do
    let ix = paramIndex p
    guard (ix >= b)
    lookupGen (ix - b) u

  where
  process         = maybe (TVar tv) (mapTVar adjust)
  adjust (GVar p) = GVar p { paramIndex = paramIndex p + b }
  adjust uv       = uv

typeVarsTVar :: TVar Kind -> Set.Set (TParam Kind)
typeVarsTVar (UVar p) = Set.singleton p
typeVarsTVar _        = Set.empty

instance Types Decl where
  apply' b s d = d { declBody = apply' b s (declBody d) }
  typeVars     = typeVars . declBody

instance Types a => Types (Forall a) where
  apply' b u (Forall ps a) = Forall ps (apply' (b + length ps) u a)
  typeVars (Forall _ a)    = typeVars a

instance Types a => Types (Qual a) where
  apply' b u (Qual cxt a) = Qual (apply' b u cxt) (apply' b u a)
  typeVars (Qual cxt a)   = typeVars cxt `Set.union` typeVars a

instance Types Match where
  apply' b s m = case m of
    MTerm t ty       -> MTerm  (apply' b s t)  (apply' b s ty)
    MSplit l r       -> MSplit (apply' b s l)  (apply' b s r)
    MPat p m'        -> MPat   (apply' b s p)  (apply' b s m')
    MGuard p e ty m' -> MGuard (apply' b s p)  (apply' b s e)
                               (apply' b s ty) (apply' b s m')
    MFail ty         -> MFail  (apply' b s ty)

  typeVars m = case m of
    MTerm t ty       -> typeVars t `Set.union` typeVars ty
    MSplit l r       -> typeVars l `Set.union` typeVars r
    MPat p m'        -> typeVars p `Set.union` typeVars m'
    MGuard p e ty m' -> Set.unions [ typeVars p,  typeVars e
                                   , typeVars ty, typeVars m'
                                   ]
    MFail ty         -> typeVars ty

instance Types Pat where
  apply' b s p = case p of
    PWildcard ty  -> PWildcard (apply' b s ty)
    PCon qn ps ty -> PCon qn   (apply' b s ps) (apply' b s ty)
    PVar v ty     -> PVar v    (apply' b s ty)

  typeVars p = case p of
    PWildcard ty -> typeVars ty
    PCon _ ps ty -> typeVars ty `Set.union` typeVars ps
    PVar _ ty    -> typeVars ty

instance Types Term where
  apply' b s tm = case tm of
    AppT f ts -> AppT (apply' b s f)  (apply' b s ts)
    App t ts  -> App  (apply' b s t)  (apply' b s ts)
    Case e m  -> Case (apply' b s e)  (apply' b s m)
    Let ds e  -> Let  (apply' b s ds) (apply' b s e)
    Global qn -> Global qn
    Local n   -> Local n
    Lit lit   -> Lit lit

  typeVars tm = case tm of
    AppT f ts -> typeVars f  `Set.union` typeVars ts
    App t ts  -> typeVars t  `Set.union` typeVars ts
    Case e m  -> typeVars e  `Set.union` typeVars m
    Let ds e  -> typeVars ds `Set.union` typeVars e
    Global _  -> Set.empty
    Local _   -> Set.empty
    Lit _     -> Set.empty

instance Types DataDecl where
  apply' b s d = d { dataGroups = map (apply' b s) (dataGroups d) }
  typeVars     = typeVars . dataGroups

instance Types ConstrGroup where
  apply' b s cg = cg
    { groupArgs    = apply' b s (groupArgs cg)
    , groupConstrs = apply' b s (groupConstrs cg)
    }

  typeVars cg = typeVars (groupArgs cg) `Set.union` typeVars (groupConstrs cg)

instance Types Constr where
  apply' b s c = c { constrFields = apply' b s (constrFields c) }

  typeVars = typeVars . constrFields


-- Unification -----------------------------------------------------------------

invalidInfixCon :: DangM m => QualName -> QualName -> m a
invalidInfixCon l r =
  do addErr $ sep [ text "Expected infix constructor" <+> quoted (ppr l)
                  , text "got" <+> quoted (ppr r) ]
     mzero

unifyError :: DangM m => Type -> Type -> m a
unifyError l r =
  do addErr $ sep [ text "The types" <+> quoted (ppr l)
                  , text "and" <+> quoted (ppr r)
                  , text "do not unify" ]
     mzero

unifyBoundSkolem :: DangM m => TParam k -> Type -> m a
unifyBoundSkolem p ty =
  do addErr $ sep [ text "Attempted to unify skolem variable" <+> quoted (ppr p)
                  , text "with type" <+> quoted (ppr ty) ]
     mzero

occursCheckFailed :: DangM m => TParam k -> Type -> m a
occursCheckFailed p ty =
  do addErr $ hang (text "Cannot construct the infinite type:")
                 2 (ppr p <+> char '=' <+> ppr ty)
     mzero

type Skolems = Set.Set (TParam Kind)

-- | Generate the most-general unifier for two types.
mgu :: DangM m => Skolems -> Type -> Type -> m Subst
mgu skolems = loop
  where
  loop a b = case (a,b) of

    -- type application
    (TApp f x, TApp g y) -> do
      sf <- loop f g
      sx <- loop (apply sf x) (apply sf y)
      return (sf @@ sx)

    -- infix type constructor application
    (TInfix n l r, TInfix m x y) -> do
      unless (n == m) (invalidInfixCon n m)
      sl <- loop l x
      sr <- loop (apply sl r) (apply sl y)
      return (sl @@ sr)

    (TVar (UVar p), r) -> varBind skolems p r
    (l, TVar (UVar p)) -> varBind skolems p l

    -- constructors
    (TCon l, TCon r) | l == r -> return emptySubst

    _ -> unifyError a b


-- | Generate a substitution that unifies a variable with a type.
varBind :: DangM m => Skolems -> TParam Kind -> Type -> m Subst
varBind skolems p ty
  | Just p' <- destUVar ty, p == p' = return emptySubst
  | p `Set.member` skolems          = unifyBoundSkolem p ty
  | occursCheck p ty                = occursCheckFailed p ty
  | otherwise                       = return (varSubst (paramIndex p) ty)


occursCheck :: TParam Kind -> Type -> Bool
occursCheck p = Set.member p . typeVars


-- Instantiation ---------------------------------------------------------------

inst :: Types t => [Type] -> t -> t
inst ts = apply (emptySubst { substBound = Map.fromList (zip [0 ..] ts) })


-- Quantification --------------------------------------------------------------

-- | Generalize type variables.
quantify :: Types t => [TParam Kind] -> t -> Forall t
quantify ps t = uncurry Forall (quantifyAux 0 ps t)

quantifyAll :: Types t => t -> Forall t
quantifyAll ty = quantify (Set.toList (typeVars ty)) ty

quantifyAux :: Types t => Int -> [TParam Kind] -> t -> ([TParam Kind],t)
quantifyAux off ps t = (ps',apply u t)
  where
  vs         = ps `intersect` Set.toList (typeVars t)
  subst      = zipWith mkGen [off ..] vs
  mkGen ix v = (paramIndex v, v { paramIndex = ix })
  (_,ps')    = unzip subst
  u          = unboundSubst (map (second gvar) subst)
