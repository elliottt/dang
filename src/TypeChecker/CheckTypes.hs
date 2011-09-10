module TypeChecker.CheckTypes where

import Dang.IO
import Pretty
import QualName
import TypeChecker.AST
import TypeChecker.Env
import TypeChecker.Monad
import TypeChecker.Types (Type(..),tarrow,kstar,Scheme,toScheme)
import qualified Syntax.AST as Syn

import Control.Applicative ((<$>))
import Control.Monad (mapAndUnzipM,forM,unless,foldM)
import Data.List (intersperse)
import qualified Data.Foldable as F


-- Type Checking ---------------------------------------------------------------

tcModule :: Syn.Module -> TC [Decl]
tcModule m = do
  let ns    = Syn.modNamespace m
      binds = map primTermSchemeBinding (Syn.modPrimTerms m)
           ++ map (typedDeclSchemeBinding ns) (Syn.modTyped m)
  env <- addPrimBinds emptyAssumps (Syn.modPrimTerms m)
  addSchemeBindings binds $ do
    mapM (tcTopTypedDecl env) (Syn.modTyped m)

addPrimBinds :: Assumps -> [Syn.PrimTerm] -> TC Assumps
addPrimBinds  = foldM addPrimBind

addPrimBind :: Assumps -> Syn.PrimTerm -> TC Assumps
addPrimBind env ptd = do
  let n = primName (Syn.primTermName ptd)
      a = Assump Nothing (Syn.primTermType ptd)

  logInfo $ concat
    [ "Introduced Type: ", Syn.primTermName ptd
    , " :: ", pretty (Syn.primTermType ptd) ]

  return (addAssump n a env)

tcTopTypedDecl :: Assumps -> Syn.TypedDecl -> TC Decl
tcTopTypedDecl env td = do
  ((ty,m),vs) <- collectVars (tcMatch env (Syn.typedBody td))

  -- there should be no variables showing up here.
  unless (null vs) $ fail $ concat
    $ "type variables escaping from the definition of ``"
    : Syn.typedName td : "''\n\t"
    : intersperse ", " (map pretty vs)

  -- fix the inferred type, given information from the signature
  oty <- freshInst (Syn.typedType td)
  unify oty ty
  ty' <- applySubst ty

  -- dump some information about the checked declaration
  logInfo (Syn.typedName td ++ " :: " ++ pretty ty')
  logInfo (Syn.typedName td ++ "  = " ++ pretty m)


{-
  dty <- inferredType (Syn.typedVars td) ty
  oty <- freshInst (Syn.typedType td)
  unify oty dty
  dty' <- applySubst dty
  logInfo (Syn.typedName td ++ " :: " ++ pretty dty') -}
  fail "typechecker borked"

tcMatch :: Assumps -> Syn.Match -> TC (Type,Match)
tcMatch env m = case m of

  Syn.MTerm t -> do
    (ty,t') <- tcTerm env t
    return (ty,MTerm t')

  Syn.MPat p m' -> tcPat env p $ \ env' pty p' -> do
    (ty,m'') <- tcMatch env' m'
    pty'     <- applySubst pty
    return (pty' `tarrow` ty, MPat p' m'')

tcPat :: Assumps -> Syn.Pat -> (Assumps -> Type -> Pat -> TC a) -> TC a
tcPat env p k = case p of

  Syn.PWildcard -> do
    v <- freshVar kstar
    k env v PWildcard

  Syn.PVar n -> do
    v <- freshVar kstar
    k (addAssump (simpleName n) (Assump Nothing (toScheme v)) env) v (PVar n)

-- | Type-check terms in the syntax into system-f like terms.
tcTerm :: Assumps -> Syn.Term -> TC (Type,Term)
tcTerm env tm = case tm of

  Syn.App f xs -> do
    (xtys,xs') <- mapAndUnzipM (tcTerm env) xs

    (fty,f') <- tcTerm env f
    res      <- freshVar kstar
    let inferred = foldr tarrow res xtys
    unify fty inferred
    return (res, App f' [] xs')

  Syn.Let{} -> fail "let"

  Syn.Abs m -> tcAbs m

  Syn.Local n -> do
    ty <- findType (simpleName n)
    emitFreeType ty
    return (ty,Local n)

  Syn.Global qn -> do
    ty <- findType qn
    return (ty,Global qn)

  Syn.Prim{} -> fail "prim"

  Syn.Lit l -> do
    (ty,l') <- tcLit l
    return (ty,Lit l')

-- | Type-check a literal.
tcLit :: Syn.Literal -> TC (Type,Syn.Literal)
tcLit l = case l of
  Syn.LInt{} -> return (TCon (primName "Int"), l)

-- | Translate an abstraction, into a let expression with a fresh name.
tcAbs :: Syn.Match -> TC (Type,Term)
tcAbs m = case m of

  Syn.MTerm b -> undefined

  Syn.MPat p m' -> undefined


-- Environment Helpers ---------------------------------------------------------

introVar :: Syn.Var -> TC (Syn.Var,Type)
introVar v = do
  ty <- freshVar kstar
  return (v,ty)

primTermSchemeBinding :: Syn.PrimTerm -> (QualName,Scheme)
primTermSchemeBinding pt =
  (primName (Syn.primTermName pt), Syn.primTermType pt)

typedDeclSchemeBinding :: Namespace -> Syn.TypedDecl -> (QualName,Scheme)
typedDeclSchemeBinding ns td =
  (qualName ns (Syn.typedName td), Syn.typedType td)
