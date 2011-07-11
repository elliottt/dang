{-# LANGUAGE TupleSections #-}

module TypeChecker.CheckTypes where

import QualName
import Syntax.AST
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify
import Variables (freeVars)

import Control.Arrow (second)
import Control.Monad (mapAndUnzipM)
import qualified Data.Set as Set


-- Variable Utilities ----------------------------------------------------------

introVar :: Var -> TC (QualName,Type)
introVar v = do
  ty <- freshVar kstar
  return (simpleName v, ty)

fullyInst :: Forall Type -> TC Type
fullyInst (Forall ps ty) = do
  vars <- mapM freshVarFromTParam ps
  inst vars ty

introUntypedDecl :: Namespace -> UntypedDecl -> TC (QualName,Type)
introUntypedDecl ns u = do
  ty <- freshVar kstar
  return (qualName ns (untypedName u), ty)


-- Generalization --------------------------------------------------------------

-- | Simple for now: just turn all variables into quantified ones.
generalize :: Type -> TC (Forall Type)
generalize ty = return (quantify (Set.toList (typeVars ty)) ty)


-- Type Checking ---------------------------------------------------------------

tcModule :: Module -> TC Module
tcModule m = do
  let ns           = qualNamespace (modName m)
      primSchemes  = map primTermBinding (modPrimTerms m)
      typedSchemes = map (typedDeclBinding ns) (modTyped m)
  addSchemeBindings (primSchemes ++ typedSchemes) $ do
    ts' <- mapM (tcTypedDecl ns) (modTyped m)
    us' <- tcUntypedDecls ns (modUntyped m)
    return m
      { modTyped   = ts' ++ us'
      , modUntyped = []
      }

primTermBinding :: PrimTerm -> (QualName,Forall Type)
primTermBinding pt = (primName (primTermName pt), primTermType pt)

typedDeclBinding :: Namespace -> TypedDecl -> (QualName,Forall Type)
typedDeclBinding ns t = (qualName ns (typedName t), typedType t)

tcTypedDecl :: Namespace -> TypedDecl -> TC TypedDecl
tcTypedDecl ns t = do
  vars  <- mapM introVar (typedVars t)
  tySig <- freshInst (typedType t)
  let name = qualName ns (typedName t)
  addTypeBindings ((name,tySig):vars) $ do
    (tyBody,b') <- tcTerm (typedBody t)
    unify tySig tyBody
    fvs <- tcFreeVars b'
    return t
      { typedFree = fvs
      , typedBody = b'
      }

tcUntypedDecls :: Namespace -> [UntypedDecl] -> TC [TypedDecl]
tcUntypedDecls ns = mapM (tcUntypedDecl ns) -- this really needs to do SCC

tcUntypedDecl :: Namespace -> UntypedDecl -> TC TypedDecl
tcUntypedDecl _ns _u = fail "tcUntypedDecl"

tcTerm :: Term -> TC (Type,Term)
tcTerm tm = case tm of

  Abs vs b -> do
    binds   <- mapM introVar vs
    (ty,b') <- addTypeBindings binds (tcTerm b)
    name    <- freshName "lambda" (freeVars b')
    let step (_,l) r = l `tarrow` r
        funTy        = foldr step ty binds
    scheme  <- generalize funTy
    let decl = TypedDecl
          { typedName   = name
          , typedExport = Private
          , typedType   = scheme
          , typedFree   = Set.empty
          , typedVars   = vs
          , typedBody   = b'
          }
    return (funTy, Let [decl] [] (Local name))

  Let ts us e -> do
    let typedSchemes = [ (simpleName (typedName t), typedType t) | t <- ts ]
    binds <- mapM (introUntypedDecl []) us
    addSchemeBindings typedSchemes $ addTypeBindings binds $ do
      ts'      <- mapM (tcTypedDecl []) ts
      us'      <- tcUntypedDecls [] us
      (ety,e') <- tcTerm e
      return (ety, Let (ts' ++ us') [] e')

  App f xs -> do
    (xtys,xs') <- mapAndUnzipM tcTerm xs
    (fty,f')   <- tcTerm f
    res        <- freshVar kstar
    unify fty (foldr tarrow res xtys)
    return (res, App f' xs')

  Local n -> (,tm) `fmap` findType (simpleName n)

  Global qn -> (,tm) `fmap` findType qn

  Lit lit -> second Lit `fmap` tcLit lit

  Prim _v -> fail "primitive"

tcLit :: Literal -> TC (Type,Literal)
tcLit l@LInt{} = return (TCon (primName "Int"), l)

tcFreeVars :: Term -> TC (Set.Set (Var,Type))
tcFreeVars  = loop Set.empty . Set.toList . freeVars
  where
  loop tvs []              = return tvs
  loop tvs (v:vs)
    | not (isSimpleName v) = loop tvs vs
    | otherwise            = do
      let n = qualSymbol v
      (ty,_) <- tcTerm (Local n)
      loop (Set.insert (n,ty) tvs) vs
