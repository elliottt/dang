{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.Unify where

import Dang.TypeChecker.Subst
import Dang.TypeChecker.Types
import Dang.Utils.Pretty

import qualified Data.Set as Set
import           MonadLib (runM,Id,ExceptionT,raise)


-- Errors ----------------------------------------------------------------------

-- | A failure during unification.
data UnifyError = UnifyError Type Type
                | MatchError Type Type
                | OccursCheckError TParam Type
                  deriving (Show,Eq)

instance Pretty UnifyError where

  ppr (UnifyError x y) =
    hang (fsep [ text "couldn't unify", quoted (pp x)
               , text "with", quoted (pp y) ])
       2 (vcat [ text "Expected type:" <+> pp x
               , text "  Actual type:" <+> pp y ])

  ppr (MatchError x y) =
    hang (fsep [ text "couldn't match", quoted (pp x)
               , text "with", quoted (pp y) ])
       2 (vcat [ text "Expected type:" <+> pp x
               , text "  Actual type:" <+> pp y ])

  ppr (OccursCheckError p ty) =
    hang (text "couldn't construct the infinite type:")
       2 (sep [pp p, char '=', pp ty])


-- Unification -----------------------------------------------------------------

type MGU = ExceptionT UnifyError Id

-- | Compute the most-general unifier between t1 and t2.
mgu :: Type -> Type -> Either UnifyError Subst
mgu t1 t2 = runM (go t1 t2)
  where

  go (TApp f x) (TApp g y) =
    do s1 <- go f g
       s2 <- go (apply s1 x) (apply s1 y)
       return (s2 @@ s1)

  go (TCon x) (TCon y)
    | x == y = return mempty

  -- bound variables only unify with themselves
  go (TGen x) (TGen y)
    | x == y = return mempty

  go (TVar p) t = varBind p t
  go t (TVar p) = varBind p t

  go x y = raise (UnifyError x y)


-- | Generate a substitution that when applied to t1 will produce t2.
match :: Type -> Type -> Either UnifyError Subst
match t1 t2 = runM (go t1 t2)
  where

  go l@(TApp f x) r@(TApp g y) =
    do s1 <- go f g
       s2 <- go x y
       case s2 `merge` s1 of
         Just s  -> return s
         Nothing -> raise (MatchError l r)

  go (TCon x) (TCon y)
    | x == y = return mempty

  -- bound variables only unify with themselves
  go (TGen x) (TGen y)
    | x == y = return mempty

  go (TVar p) t = varBind p t

  go x y = raise (MatchError x y)


-- | Bind a free variable.
varBind :: TParam -> Type -> MGU Subst
varBind p ty
  | pty == ty                   = return mempty
  | p `Set.member` freeTVars ty = raise (OccursCheckError p ty)
  | otherwise                   = return (listFreeSubst [(p,ty)])

  where
  pty = TVar p


-- Free Type Variables ---------------------------------------------------------

class TypeVars a where
  freeTVars :: a -> Set.Set TParam

instance TypeVars Type where
  freeTVars (TApp l r) = Set.union (freeTVars l) (freeTVars r)
  freeTVars (TVar p)   = Set.singleton p
  freeTVars TCon{}     = Set.empty
  freeTVars TGen{}     = Set.empty
