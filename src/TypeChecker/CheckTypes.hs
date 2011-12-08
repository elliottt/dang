{-# LANGUAGE DoRec #-}

module TypeChecker.CheckTypes where

import Core.AST
import Dang.IO
import Dang.Monad
import ModuleSystem.Interface (HasInterface,getTypes)
import Pretty
import QualName
import TypeChecker.Env
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify (quantify,typeVars,Types)
import Variables (freeVars,sccFreeNames,sccToList)
import qualified Syntax.AST as Syn

import Control.Monad (foldM,mapAndUnzipM)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map


type TypeAssumps = Assumps Scheme

assume :: QualName -> Scheme -> TypeAssumps -> TC TypeAssumps
assume qn qt env = do
  logInfo ("  Assuming: " ++ pretty qn ++ " :: " ++ pretty qt)
  return (addAssump qn (Assump Nothing qt) env)

-- | Turn an interface into an initial set of assumptions.
interfaceAssumps :: HasInterface iset => iset -> TypeAssumps
interfaceAssumps  = foldl step emptyAssumps . getTypes
  where
  step env (qn,sc) = addAssump qn (Assump Nothing sc) env

-- | Lookup a type assumption in the environment.
typeAssump :: QualName -> TypeAssumps -> TC (Assump Scheme)
typeAssump qn env = case lookupAssump qn env of
  Just a  -> return a
  Nothing -> raiseE (UnboundIdentifier qn)

-- | Add primitive terms to the typing environment.
primAssumps :: Namespace -> [Syn.PrimTerm] -> TypeAssumps -> TC TypeAssumps
primAssumps ns pts env0 = foldM step env0 pts
  where
  step env pt =
    assume (primName ns (Syn.primTermName pt)) (Syn.primTermType pt) env

-- | Introduce the signatures as the assumed type for a block of typed
-- declarations.
typedAssumps :: Namespace -> [Syn.TypedDecl] -> TypeAssumps -> TC TypeAssumps
typedAssumps ns ts env0 = foldM step env0 ts
  where
  step env td = assume (qualName ns (Syn.typedName td)) (Syn.typedType td) env


-- Modules ---------------------------------------------------------------------

-- | Type-check a module, producing a list of fully-qualified declarations.
tcModule :: HasInterface i => i -> Syn.Module -> TC Module
tcModule i m = do
  logInfo ("Checking module: " ++ pretty (Syn.modName m))
  let ns = Syn.modNamespace m
  env <- typedAssumps   ns (Syn.modTyped m)   =<<
         primAssumps ns (Syn.modPrimTerms m) (interfaceAssumps i)

  (env',us) <- tcUntypedDecls ns env (Syn.modUntyped m)
  ts        <- mapM (tcTypedDecl ns env') (Syn.modTyped m)

  return (emptyModule (Syn.modName m))
    { modDecls     = ts ++ us
    , modPrimTypes = Syn.modPrimTypes m
    , modPrimTerms = Syn.modPrimTerms m
    }


-- Typed Declarations ----------------------------------------------------------

-- | Check a typed declaration.
tcTypedDecl :: Namespace -> TypeAssumps -> Syn.TypedDecl -> TC Decl
tcTypedDecl ns env td = do
  let name = qualName ns (Syn.typedName td)
  logInfo ("Checking: " ++ pretty name)

  sig    <- freshInst (Syn.typedType td)
  (ty,m) <- tcMatch env (Syn.typedBody td)

  unify ty sig
  m' <- applySubst m

  let ps = Set.toList (genVars env m')
  return Decl
    { declName   = name
    , declExport = Syn.typedExport td
    , declBody   = quantify ps m'
    }


-- Untyped Declarations --------------------------------------------------------

-- | Check a group of untyped declarations.
tcUntypedDecls :: Namespace -> TypeAssumps -> [Syn.UntypedDecl]
               -> TC (TypeAssumps,[Decl])
tcUntypedDecls ns env0 us = foldM step (env0,[]) (sccFreeNames ns us)
  where
  step (env,ds) scc = do
    (env',us') <- tcUntypedDeclBlock ns env (sccToList scc)
    return (env', us' ++ ds)

-- | Check a block of untyped declarations, and return a new typing environment
--   with their generalized types contained.
tcUntypedDeclBlock :: Namespace -> TypeAssumps -> [Syn.UntypedDecl]
                   -> TC (TypeAssumps, [Decl])
tcUntypedDeclBlock ns envGen us = do
  rec let bodies  = partialBodies envGen pds
      envInf <- untypedAssumps ns bodies us envGen
      pds    <- mapM (tcUntypedDecl ns envInf) us

  envGen' <- applySubst envGen
  return (finalizePartialDecls envGen' pds)

-- | Check an untyped declaration, producing a partially complete declaration.
tcUntypedDecl :: Namespace -> TypeAssumps -> Syn.UntypedDecl -> TC PartialDecl
tcUntypedDecl ns envInf u = do
  let name = qualName ns (Syn.untypedName u)
  logInfo ("Inferring: " ++ pretty name)

  (ty,m) <- tcMatch envInf (Syn.untypedBody u)
  a      <- typeAssump name envInf

  -- At this point, the environment holds a single unification variable as the
  -- type of this declaration.  Pull it out, and unify with the inferred type.
  let Forall [] var = aData a
  unify var ty

  ty' <- applySubst ty
  m'  <- applySubst m

  return PartialDecl
    { partialName   = name
    , partialExport = Syn.untypedExport u
    , partialType   = ty'
    , partialBody   = m'
    }


-- Partial Untyped Declarations ------------------------------------------------

type Bodies = Map.Map QualName Term

-- | Generate the map from names to new declarations.
partialBodies :: TypeAssumps -> [PartialDecl] -> Bodies
partialBodies envGen = Map.fromList . map mk
  where
  mk d = (name, appT (Global name) (map uvar ps))
    where
    ps   = Set.toList (genVars envGen (partialType d))
    name = partialName d

-- | Introduce fresh variables for the types of a block of untyped declarations.
untypedAssumps :: Namespace -> Bodies -> [Syn.UntypedDecl] -> TypeAssumps
               -> TC TypeAssumps
untypedAssumps ns bodies us env0 = foldM step env0 us
  where
  step env u = do
    var <- freshVar kstar
    let name   = qualName ns (Syn.untypedName u)
        assump = Assump
          { aBody = Map.lookup name bodies
          , aData = toScheme var
          }
    return (addAssump name assump env)

data PartialDecl = PartialDecl
  { partialName   :: QualName
  , partialExport :: Syn.Export
  , partialType   :: Type
  , partialBody   :: Match
  } deriving Show

finalizePartialDecls :: TypeAssumps -> [PartialDecl] -> (TypeAssumps,[Decl])
finalizePartialDecls env0 = foldl extendAndFinalize (env0,[])
  where
  extendAndFinalize (env,ds) pd = (env',d:ds)
    where
    (env',d) = finalizePartialDecl env pd

finalizePartialDecl :: TypeAssumps -> PartialDecl -> (TypeAssumps,Decl)
finalizePartialDecl env pd = (addAssump name assump env,decl)
  where
  name = partialName pd
  body = partialBody pd
  ps   = Set.toList (genVars env body)

  decl = Decl
    { declName   = name
    , declExport = partialExport pd
    , declBody   = quantify ps body
    }

  assump = Assump
    { aBody = Nothing
    , aData = quantify ps (partialType pd)
    }



-- Terms -----------------------------------------------------------------------

tcMatch :: TypeAssumps -> Syn.Match -> TC (Type,Match)
tcMatch env m = case m of

  Syn.MPat p m' -> do
    (env',pty,p') <- tcPat env p
    (ty,m'')      <- tcMatch env' m'
    return (pty `tarrow` ty, MPat p' m'')

  Syn.MTerm tm -> do
    (ty,tm') <- tcTerm env tm
    return (ty, MTerm tm' ty)

tcPat :: TypeAssumps -> Syn.Pat -> TC (TypeAssumps,Type,Pat)
tcPat env p = case p of

  Syn.PWildcard -> do
    var <- freshVar kstar
    return (env,var,PWildcard var)

  Syn.PVar n -> do
    let name = simpleName n
    var  <- freshVar kstar
    env' <- assume name (toScheme var) env
    return (env',var,PVar n var)

tcTerm :: TypeAssumps -> Syn.Term -> TC (Type,Term)
tcTerm env tm = case tm of

  Syn.Abs m -> do
    (ty,m') <- tcMatch env m
    n       <- freshName "_lam" (freeVars m')
    let qvs  = Set.toList (genVars env m')
        body = quantify qvs m'
        qt   = quantify qvs ty
        name = simpleName n
        decl = Decl
          { declName   = name
          , declExport = Syn.Private
          , declBody   = body
          }

    (vs,ty') <- freshInst' qt
    let vars = map uvar vs
    return (ty', Let [decl] (appT (Global name) vars))

  Syn.Let ts us e -> do
    env'        <- typedAssumps [] ts env
    (env'',us') <- tcUntypedDecls [] env' us
    ts'         <- mapM (tcTypedDecl [] env'') ts
    (ty,e')     <- tcTerm env'' e
    return (ty, Let (ts' ++ us') e')

  Syn.App f xs -> do
    (fty,f')   <- tcTerm env f
    (xtys,xs') <- mapAndUnzipM (tcTerm env) xs

    res <- freshVar kstar
    let inferred = foldr tarrow res xtys
    unify inferred fty

    f''  <- applySubst f'
    xs'' <- applySubst xs'
    ty   <- applySubst res

    return (ty, App f'' xs'')

  Syn.Local n -> do
    let name = simpleName n
    a       <- typeAssump name env
    (ps,ty) <- freshInst' (aData a)
    let body = fromMaybe (Local n) (aBody a)
    return (ty, appT body (map uvar ps))

  Syn.Global qn -> do
    a       <- typeAssump qn env
    (ps,ty) <- freshInst' (aData a)
    let body = fromMaybe (Global qn) (aBody a)
    return (ty, appT body (map uvar ps))

  Syn.Lit lit -> tcLit lit

tcLit :: Syn.Literal -> TC (Type,Term)
tcLit l = case l of
  Syn.LInt{} -> return (TCon (primName ["Prim","Type"] "Int"), Lit l)


-- Generalization --------------------------------------------------------------

-- | Variables from the assumptions.
assumpVars :: TypeAssumps -> Set.Set TParam
assumpVars  = typeVars . assumps

-- | Variables that can be generalized.
genVars :: Types t => TypeAssumps -> t -> Set.Set TParam
genVars env t = typeVars t Set.\\ assumpVars env
