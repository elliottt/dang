module TypeChecker.CheckTypes where

import Dang.IO
import Interface (IsInterface,funSymbols,funType)
import Pretty
import QualName
import TypeChecker.AST
import TypeChecker.Env
import TypeChecker.Monad
import TypeChecker.Types
import TypeChecker.Unify (quantify,typeVars,Types)
import Variables (freeVars)
import qualified Syntax.AST as Syn

import Control.Monad (foldM,mapAndUnzipM)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set


type TypeAssumps = Assumps Scheme

assume :: QualName -> Scheme -> TypeAssumps -> TC TypeAssumps
assume qn qt env = do
  logInfo ("  Assuming: " ++ pretty qn ++ " :: " ++ pretty qt)
  return (addAssump qn (Assump Nothing qt) env)

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

-- | Add primitive terms to the typing environment.
primAssumps :: [Syn.PrimTerm] -> TypeAssumps -> TC TypeAssumps
primAssumps pts env0 = foldM step env0 pts
  where
  step env pt =
    assume (primName (Syn.primTermName pt)) (Syn.primTermType pt) env

-- | Introduce the signatures as the assumed type for a block of typed
-- declarations.
typedAssumps :: Namespace -> [Syn.TypedDecl] -> TypeAssumps -> TC TypeAssumps
typedAssumps ns ts env0 = foldM step env0 ts
  where
  step env td = assume (qualName ns (Syn.typedName td)) (Syn.typedType td) env

-- | Introduce fresh variables for the types of a block of untyped declarations.
untypedAssumps :: Namespace -> [Syn.UntypedDecl] -> TypeAssumps
               -> TC TypeAssumps
untypedAssumps ns us env0 = foldM step env0 us
  where
  step env ud = do
    var <- freshVar kstar
    assume (qualName ns (Syn.untypedName ud)) (toScheme var) env


-- Type Checking ---------------------------------------------------------------

-- | Type-check a module, producing a list of fully-qualified declarations.
tcModule :: IsInterface i => i -> Syn.Module -> TC [Decl]
tcModule i m = do
  logInfo ("Checking module: " ++ pretty (Syn.modName m))
  let ns = Syn.modNamespace m
  env <- typedAssumps   ns (Syn.modTyped m)   =<<
         untypedAssumps ns (Syn.modUntyped m) =<<
         primAssumps (Syn.modPrimTerms m) (interfaceAssumps i)

  ts <- mapM (tcTypedDecl ns env) (Syn.modTyped m)
  us <- tcUntypedDecls ns env (Syn.modUntyped m)

  return (ts ++ us)


tcTypedDecl :: Namespace -> TypeAssumps -> Syn.TypedDecl -> TC Decl
tcTypedDecl ns env td = do
  let name = qualName ns (Syn.typedName td)
  logInfo ("Checking: " ++ pretty name)

  sig    <- freshInst (Syn.typedType td)
  (ty,m) <- tcMatch env (Syn.typedBody td)

  logInfo ("  Inferred: " ++ pretty ty)
  unify ty sig

  m' <- applySubst m

  let ps = Set.toList (genVars env m')
  return (Decl name (quantify ps m'))

tcUntypedDecls :: Namespace -> TypeAssumps -> [Syn.UntypedDecl] -> TC [Decl]
tcUntypedDecls _ns _env _us = do
  logError "tcModule: only checking typed declarations"
  return []

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
    var  <- freshVar kstar
    env' <- assume (simpleName n) (toScheme var) env
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
        decl = Decl name body

    (vs,ty') <- freshInst' qt
    let vars = map TVar vs
    return (ty', Let [decl] (appT (Global name) vars))

  Syn.Let ts us e -> do
    env'    <- untypedAssumps [] us =<< typedAssumps [] ts env
    ts'     <- mapM (tcTypedDecl [] env') ts
    us'     <- tcUntypedDecls [] env' us
    (ty,e') <- tcTerm env' e
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
    a       <- typeAssump (simpleName n) env
    (ps,ty) <- freshInst' (aData a)
    let body = fromMaybe (Local n) (aBody a)
    return (ty, appT body (map TVar ps))

  Syn.Global qn -> do
    a       <- typeAssump qn env
    (ps,ty) <- freshInst' (aData a)
    let body = fromMaybe (Global qn) (aBody a)
    return (ty, appT body (map TVar ps))

  Syn.Lit lit -> tcLit lit

appT :: Term -> [Type] -> Term
appT tm [] = tm
appT tm ts = AppT tm ts

tcLit :: Syn.Literal -> TC (Type,Term)
tcLit l = case l of
  Syn.LInt{} -> return (TCon (primName "Int"), Lit l)


-- Generalization --------------------------------------------------------------

-- | Variables from the assumptions.
assumpVars :: TypeAssumps -> Set.Set TParam
assumpVars  = typeVars . assumps

-- | Variables that can be generalized.
genVars :: Types t => TypeAssumps -> t -> Set.Set TParam
genVars env t = typeVars t Set.\\ assumpVars env
