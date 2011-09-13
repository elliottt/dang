{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.CheckTypes where

import Dang.IO
import Dang.Monad
import Interface (IsInterface,funSymbols,funType)
import Pretty
import QualName
import TypeChecker.AST
import TypeChecker.Env
import TypeChecker.Monad
import TypeChecker.Types
    (Type(..),tarrow,kstar,Scheme,toScheme,Forall(..),destArgs,TParam)
import TypeChecker.Unify (quantify,quantifyAll,typeVars)
import Variables (freeVars)
import qualified Data.ClashMap as CM
import qualified Syntax.AST as Syn

import Control.Monad (mapAndUnzipM,foldM,unless)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import qualified Data.Set as Set


type TypeAssumps = Assumps Scheme

-- | Turn an interface into an initial set of assumptions.
interfaceAssumps :: IsInterface iset => iset -> TypeAssumps
interfaceAssumps  = foldl step emptyAssumps . funSymbols
  where
  step env (qn,sym) = addAssump qn (Assump Nothing (funType sym)) env

-- | Lookup a type assumption in the environment.
typeAssump :: QualName -> TypeAssumps -> TC (Assump Scheme)
typeAssump qn env = case lookupAssump qn env of
  Just a  -> return a
  Nothing -> unboundIdentifier qn


-- Type Checking ---------------------------------------------------------------

-- | Add a series of primitive bindings to an environment.
addPrimBinds :: [Syn.PrimTerm] -> TypeAssumps -> TC TypeAssumps
addPrimBinds  = flip (foldM addPrimBind)

-- | Add a primitive term binding to the environment.
addPrimBind :: TypeAssumps -> Syn.PrimTerm -> TC TypeAssumps
addPrimBind env ptd = do
  let n = primName (Syn.primTermName ptd)
      a = Assump Nothing (Syn.primTermType ptd)

  logInfo $ concat
    [ "  Assuming: ", pretty n
    , " :: ", pretty (Syn.primTermType ptd) ]

  return (addAssump n a env)

addTypeSigs :: Namespace -> [Syn.TypedDecl] -> TypeAssumps -> TC TypeAssumps
addTypeSigs ns = flip (foldM (addTypeSig ns))

addTypeSig :: Namespace -> TypeAssumps -> Syn.TypedDecl -> TC TypeAssumps
addTypeSig ns env td = do
  let n = qualName ns (Syn.typedName td)
      a = Assump Nothing (Syn.typedType td)
  logInfo $ concat
    [ "  Assuming: ", pretty n
    , " :: ", pretty (Syn.typedType td) ]

  return (addAssump n a env)

-- | Type-check a module.
tcModule :: IsInterface iset => iset -> Syn.Module -> TC [Decl]
tcModule iset m = do
  logInfo ("Checking module: " ++ pretty (Syn.modName m))
  let ns = Syn.modNamespace m
  env <- addTypeSigs ns (Syn.modTyped m)
         =<< addPrimBinds (Syn.modPrimTerms m) (interfaceAssumps iset)
  mapM (tcTopTypedDecl ns env) (Syn.modTyped m)

-- | Type-check a top-level, typed declaration.
tcTopTypedDecl :: Namespace -> TypeAssumps -> Syn.TypedDecl -> TC Decl
tcTopTypedDecl ns env td = do
  ((vs,ty,m),fvs) <- collectVars (tcMatch env (Syn.typedBody td))

  -- this should be caught by the module system, but if not, catch it here.
  unless (Set.null fvs) (unexpectedFreeVars fvs)

  -- fix the inferred type, given information from the signature
  oty <- freshInst (Syn.typedType td)
  unify oty ty
  ty' <- applySubst ty
  m'  <- applySubst m

  -- generate the type-variables needed for this declaration
  let vars = Set.toList (typeVars ty')
      name = qualName ns (Syn.typedName td)
      decl = Decl
        { declName = name
        , declBody = quantify vars m'
        }

  -- dump some information about the checked declaration
  logInfo (pretty name ++ " :: " ++ pretty (quantifyAll ty'))
  logInfo (pretty decl)
  return decl

-- | Type-check a variable introduction.
tcMatch :: TypeAssumps -> Syn.Match -> TC ([TParam],Type,Match)
tcMatch env m = case m of

  Syn.MTerm t -> do
    (Forall vs ty,t') <- tcTerm env t
    ty'     <- applySubst ty
    return (vs, ty', MTerm t' ty')

  Syn.MPat p m' -> tcPat env p $ \ env' pty p' -> do
    (vs,ty,m'') <- tcMatch env' m'
    pty'        <- applySubst pty
    p''         <- applySubst p'
    return (vs, pty' `tarrow` ty, MPat p'' m'')

-- | Type-check a pattern.
tcPat :: TypeAssumps -> Syn.Pat -> (TypeAssumps -> Type -> Pat -> TC a) -> TC a
tcPat env p k = case p of

  Syn.PWildcard -> do
    v <- freshVar kstar
    k env v (PWildcard v)

  Syn.PVar n -> do
    v <- freshVar kstar
    k (addAssump (simpleName n) (Assump Nothing (toScheme v)) env) v (PVar n v)

-- | Type-check terms in the syntax into system-f like terms.
tcTerm :: TypeAssumps -> Syn.Term -> TC (Scheme,Term)
tcTerm env tm = case tm of

  Syn.App f xs -> tcApp env f xs

  Syn.Let ts us e -> tcLet env ts us e

  Syn.Abs m -> tcAbs env m

  Syn.Local n -> do
    a <- typeAssump (simpleName n) env
    return (aData a, fromMaybe (Local n) (aBody a))

  Syn.Global qn -> do
    a <- typeAssump qn env
    return (aData a, fromMaybe (Global qn) (aBody a))

  Syn.Prim{} -> fail "prim"

  Syn.Lit l -> do
    (ty,l') <- tcLit l
    return (ty,Lit l')

tcApp :: TypeAssumps -> Syn.Term -> [Syn.Term] -> TC (Scheme,Term)
tcApp env f xs = do
  (xtys,xs')           <- mapAndUnzipM (tcTerm env) xs
  (Forall vars fty,f') <- tcTerm env f
  logInfo (pretty vars)
  logInfo (pretty fty)
  fail "thingy"


-- | Type-check a let expression.
tcLet :: TypeAssumps -> [Syn.TypedDecl] -> [Syn.UntypedDecl] -> Syn.Term
      -> TC (Scheme,Term)
tcLet env ts us e = do
  env0       <- addNameVars env (map Syn.typedName ts ++ map Syn.untypedName us)
  (env1,ts') <- tcTypedDecls env0 ts
  us'        <- tcUntypedDecls env0 us
  (ty,e')    <- tcTerm env0 e
  return (ty, Let (reverse ts' ++ us') e')

-- | Introduce fresh type variables for all names in a block of bindings.
addNameVars :: TypeAssumps -> [Name] -> TC TypeAssumps
addNameVars = foldM $ \ env n -> do
  v <- freshVar kstar
  return (addAssump (simpleName n) (Assump Nothing (toScheme v)) env)

-- | Type-check a block of typed declarations.
tcTypedDecls :: TypeAssumps -> [Syn.TypedDecl] -> TC (TypeAssumps,[Decl])
tcTypedDecls env0 = foldM step (env0,[])
  where
  step (env,tds) td = do
    (env',td') <- tcTypedDecl env td
    return (env',td':tds)

-- | Type-check a typed declaration that shows up in a let-expression.
tcTypedDecl :: TypeAssumps -> Syn.TypedDecl -> TC (TypeAssumps,Decl)
tcTypedDecl env td = do
  ((vs,ty,m),fvs) <- collectVars (tcMatch env (Syn.typedBody td))

  oty <- freshInst (Syn.typedType td)
  unify oty ty
  ty' <- applySubst ty
  m'  <- applySubst m

  let vars = Set.toList (typeVars ty')
      name = simpleName (Syn.typedName td)
      decl = Decl
        { declName = name
        , declBody = quantify vars m'
        }

      env' = addAssump name (Assump Nothing (quantifyAll ty)) env

  return (env, decl)

-- | Type-check a block of untyped declarations that show up in a
-- let-expression.
tcUntypedDecls :: TypeAssumps -> [Syn.UntypedDecl] -> TC [Decl]
tcUntypedDecls _env _us = do
  logError "tcUntypedDecls not implemented"
  return []

-- | Translate an abstraction, into a let expression with a fresh name.
tcAbs :: TypeAssumps -> Syn.Match -> TC (Scheme,Term)
tcAbs env m = do
  (vs,ty,m') <- tcMatch env m
  lam        <- freshName "_lam" (freeVars m')
  let decl = Decl (simpleName lam) (Forall vs m')
  return (Forall vs ty, Let [decl] (Local lam))

-- | Type-check a literal.
tcLit :: Syn.Literal -> TC (Scheme,Syn.Literal)
tcLit l = case l of
  Syn.LInt{} -> return (toScheme (TCon (primName "Int")), l)


-- Errors ----------------------------------------------------------------------

data TypeCheckingError
  = UnexpectedFreeVars FreeVars
    deriving (Show,Typeable)

instance Exception TypeCheckingError

unexpectedFreeVars :: FreeVars -> TC a
unexpectedFreeVars  = raiseE . UnexpectedFreeVars
