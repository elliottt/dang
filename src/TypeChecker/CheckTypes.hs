{-# LANGUAGE TupleSections #-}

module TypeChecker.CheckTypes where

import Dang.IO
import Pretty (pretty)
import QualName
import Syntax.AST
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify

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

introDecl :: Namespace -> Decl -> TC (QualName,Type)
introDecl ns d = do
  ty <- freshVar kstar
  return (qualName ns (declName d), ty)


-- Type Checking ---------------------------------------------------------------

tcModule :: Module -> TC Module
tcModule m = addSchemeBindings (map primTermBinding (modPrimTerms m)) $ do

  let ns = qualNamespace (modName m)
  decls' <- tcDecls ns (modDecls m)

  return m
    { modDecls = decls'
    }

primTermBinding :: PrimTerm -> (QualName,Forall Type)
primTermBinding pt = (primName (primTermName pt), primTermType pt)

tcDecls :: Namespace -> [Decl] -> TC [Decl]
tcDecls ns ds = do
  binds <- mapM (introDecl ns) ds
  addTypeBindings binds (mapM (fmap snd . tcDecl ns) ds)


tcDecl :: Namespace -> Decl -> TC (Type,Decl)
tcDecl ns d = do
  let qn = qualName ns (declName d)
  var <- findType qn

  ty <- case declType d of
    Nothing     -> return var
    Just scheme -> do
      ty <- fullyInst scheme
      unify var ty
      return ty

  binds    <- mapM introVar (declVars d)
  (dty,b') <- addTypeBindings binds (tcTerm (declBody d))
  let declType = foldr tarrow dty (map snd binds)
  unify ty declType
  x <- applySubst declType
  logInfo (pretty qn ++ " :: " ++ pretty x)

  var' <- applySubst var
  let scheme = quantify (Set.toList (typeVars var')) var'
  logDebug (declName d ++ " :: " ++ pretty scheme)
  return (var', d { declType = Just scheme, declBody = b' })

tcTerm :: Term -> TC (Type,Term)
tcTerm tm = case tm of

  Abs vs b -> do
    binds   <- mapM introVar vs
    (ty,b') <- addTypeBindings binds (tcTerm b)
    let step (_,l) r = l `tarrow` r
    return (foldr step ty binds, Abs vs b')

  Let ds e -> do
    binds <- mapM (introDecl []) ds
    addTypeBindings binds $ do
      (_dtys,ds') <- mapAndUnzipM (tcDecl []) ds
      (ety,e')    <- tcTerm e
      return (ety, Let ds' e')

  App f xs -> do
    (xtys,xs') <- mapAndUnzipM tcTerm xs
    (fty,f')   <- tcTerm f
    res        <- freshVar kstar
    unify fty (foldr tarrow res xtys)
    return (res, App f' xs')

  Local n -> (,tm) `fmap` findType (simpleName n)

  Global qn -> (,tm) `fmap` findType qn

  Lit lit -> second Lit `fmap` tcLit lit

  Prim v -> undefined

tcLit :: Literal -> TC (Type,Literal)
tcLit l@LInt{} = return (TCon (primName "Int"), l)
