{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker.Unify where

import Dang.Monad
import Pretty
import TypeChecker.AST
import TypeChecker.Types
import Utils ((!!?))

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import MonadLib (ExceptionM)
import qualified Data.Set as Set


newtype Subst = Subst { unSubst :: [(Index,Type)] }
    deriving (Show)

instance Pretty Subst where
  pp _ (Subst us) = braces (commas (map step us))
    where
    step (p,ty) = ppr p <+> text "+->" <+> ppr ty

class Types a where
  apply    :: Subst -> a -> a
  typeVars :: a -> Set.Set (Index,TParam)

instance Types a => Types [a] where
  apply u  = map (apply u)
  typeVars = Set.unions . map typeVars

instance Types Type where
  apply s ty = case ty of
    TApp f x     -> TApp (apply s f) (apply s x)
    TInfix n l r -> TInfix n (apply s l) (apply s r)
    TVar i _     -> fromMaybe ty (lookupSubst i s)
    TGen{}       -> ty
    TCon{}       -> ty

  typeVars ty = case ty of
    TApp f x     -> typeVars f `Set.union` typeVars x
    TInfix _ l r -> typeVars l `Set.union` typeVars r
    TVar i p     -> Set.singleton (i,p)
    TGen{}       -> Set.empty
    TCon{}       -> Set.empty

instance Types Decl where
  apply s d  = d { declBody = apply s (declBody d) }
  typeVars d = typeVars (declBody d)

instance Types a => Types (Forall a) where
  apply s (Forall ps a) = Forall ps (apply s a)
  typeVars (Forall _ a) = typeVars a

instance Types Match where
  apply s m = case m of
    MTerm t ty -> MTerm t (apply s ty)
    MPat p m'  -> MPat (apply s p) m'

  typeVars m = case m of
    MTerm t ty -> typeVars t `Set.union` typeVars ty
    MPat p m'  -> typeVars p `Set.union` typeVars m'

instance Types Pat where
  apply s p = case p of
    PWildcard ty -> PWildcard (apply s ty)
    PVar v ty    -> PVar v (apply s ty)

  typeVars p = case p of
    PWildcard ty -> typeVars ty
    PVar _ ty    -> typeVars ty

instance Types Term where
  apply s tm = case tm of
    AbsT vs b -> AbsT vs (apply s b)
    AppT f ts -> AppT (apply s f) (apply s ts)
    App t ts  -> App (apply s t)  (apply s ts)
    Let ds e  -> Let (apply s ds) (apply s e)
    Local v   -> Local v
    Global qn -> Global qn
    Lit lit   -> Lit lit

  typeVars tm = case tm of
    AbsT _ b  -> typeVars b
    AppT f ts -> typeVars f `Set.union` typeVars ts
    App t ts  -> typeVars t `Set.union` typeVars ts
    Let ds e  -> typeVars ds `Set.union` typeVars e
    Local _   -> Set.empty
    Global _  -> Set.empty
    Lit _     -> Set.empty

lookupSubst :: Index -> Subst -> Maybe Type
lookupSubst p (Subst u) = lookup p u

emptySubst :: Subst
emptySubst  = Subst []

-- | Generate a singleton substitution.
(+->) :: Index -> Type -> Subst
v +-> ty = Subst [(v,ty)]

-- | Compose two substitutions.
(@@) :: Subst -> Subst -> Subst
s1 @@ Subst s2 = Subst ([ (p,apply s1 ty) | (p,ty) <- s2 ] ++ unSubst s1)

data UnifyError
  = UnifyError Type Type
  | UnifyOccursCheck TParam Type
  | UnifyGeneric String
    deriving (Show,Typeable)

instance Exception UnifyError

unifyError :: ExceptionM m SomeException => String -> m a
unifyError  = raiseE . UnifyGeneric

-- | Generate the most-general unifier for two types.
mgu :: ExceptionM m SomeException => Type -> Type -> m Subst
mgu a b = case (a,b) of

  -- type application
  (TApp f x, TApp g y) -> do
    sf <- mgu f g
    sx <- mgu x y
    return (sf @@ sx)

  -- infix type constructor application
  (TInfix n l r, TInfix m x y) -> do
    unless (n == m) $ unifyError $ concat
      [ "Expected infix constructor ``", pretty n
      , "'', got ``", pretty m, "''" ]
    sl <- mgu l x
    sr <- mgu r y
    return (sl @@ sr)

  (TVar i p, r) -> varBind i p r
  (l, TVar i p) -> varBind i p l

  -- constructors
  (TCon l, TCon r) | l == r -> return emptySubst

  -- how do we handle nats here?

  _ -> raiseE (UnifyError a b)


-- | Generate a substitution that unifies a variable with a type.
--
-- XXX should this do a kind check in addition to an occurs check?
varBind :: ExceptionM m SomeException => Index -> TParam -> Type -> m Subst
varBind i p ty
  | isTVar ty          = return emptySubst
  | occursCheck i p ty = raiseE (UnifyOccursCheck p ty)
  | otherwise          = return (i +-> ty)


occursCheck :: Index -> TParam -> Type -> Bool
occursCheck i p = Set.member (i,p) . typeVars


-- Instantiation ---------------------------------------------------------------

data InstError = InstError TParam
    deriving (Show,Typeable)

instance Exception InstError

class Instantiate t where
  inst :: ExceptionM m SomeException => [Type] -> t -> m t

instance Instantiate a => Instantiate [a] where
  inst ts = mapM (inst ts)

instance Instantiate Type where
  inst ts (TApp l r) = do
    l' <- inst ts l
    r' <- inst ts r
    return (TApp l' r')

  inst ts (TInfix n l r) = do
    l' <- inst ts l
    r' <- inst ts r
    return (TInfix n l' r')

  inst ts (TGen n p) = case ts !!? n of
    Nothing -> raiseE (InstError p)
    Just ty -> return ty

  inst _ ty = return ty


-- Quantification --------------------------------------------------------------

quantify :: Types t => [(Index,TParam)] -> t -> Forall t
quantify ps t = Forall (map snd vs) (apply s t)
  where
  vs = [ v | v <- Set.toList (typeVars t), v `elem` ps ]
  s  = Subst (zipWith (\n (i,p) -> (i,TGen n p)) [0 ..] vs)

quantifyAll :: Types t => t -> Forall t
quantifyAll ty = quantify (Set.toList (typeVars ty)) ty
