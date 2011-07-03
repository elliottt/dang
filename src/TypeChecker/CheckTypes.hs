module TypeChecker.CheckTypes where

import QualName
import Syntax.AST
import TypeChecker.Assume
import TypeChecker.Monad
import TypeChecker.Types

import MonadLib
import qualified Data.Foldable as F


-- Utilities -------------------------------------------------------------------

type TypeEnv = Assumps Type

-- | Introduce a new type variable for each variable given, and augment the
-- typing environment with that.
introVar :: TypeEnv -> Var -> TC (TypeEnv,Type)
introVar env v = do
  ty <- freshVar kstar
  return (addAssump (Assume (simpleName v) ty) env,ty)

-- | Introduce many variables, returning a modified typing environment, and a
-- list of introduced types.
introVars :: TypeEnv -> [Var] -> TC (TypeEnv,[Type])
introVars env0 = F.foldrM step (env0,[])
  where
  step v (env,ts) = do
    (env',t) <- introVar env v
    return (env',t:ts)

-- | Lookup a name in the typing environment.
findType :: QualName -> TypeEnv -> TC Type
findType qn env = applySubst =<< findAssump qn env


-- Type Checking ---------------------------------------------------------------

tcTerm :: TypeEnv -> Term -> TC (Type,Term)
tcTerm env tm = case tm of

  Abs vs b -> do
    (env',vars) <- introVars env vs
    (bty,b')    <- tcTerm env' b
    return (foldr tarrow bty vars, b')

  Let ds e -> undefined

  App f xs -> do
    (fty,f')   <- tcTerm env f
    (xtys,xs') <- mapAndUnzipM (tcTerm env) xs
    res        <- freshVar kstar
    let ty = foldr tarrow res xtys
    unify fty ty
    res' <- applySubst res
    return (res', App f' xs')

  Local n -> undefined

  Global qn -> undefined

  Lit lit -> undefined

  Prim v -> undefined
