{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Dang.ModuleSystem.Rename (
    Renamed,
    renameModule,
  ) where

import Dang.AST
import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.ModuleSystem.Env
import Dang.ModuleSystem.Name hiding (modName)
import Dang.Unique (withSupply)
import Dang.Utils.Ident (Namespace,packNamespace,dot)
import Dang.Utils.PP
import Dang.Utils.Panic

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import           Control.Lens (Lens',over,view)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)
import           MonadLib (runM,BaseM(..),StateT,get,sets,sets_)


data Renamed

type instance IdentOf  Renamed = Name
type instance TypeOf   Renamed = Type Renamed
type instance SchemaOf Renamed = Schema Renamed
type instance MetaOf   Renamed = SourceRange


-- | Rename a top-level module.
renameModule :: HasCallStack => Module Parsed -> Dang (Module Renamed)
renameModule m =
  rename (pnameNamespace (modName m)) (rnTopModule m)


-- Renaming Monad --------------------------------------------------------------

type Names = NameTrie [Name]

data Scope = Scope { scopeNS      :: !Namespace -- ^ Fully-qualified namespace
                   , scopePrefix  :: [T.Text]   -- ^ Relative prefix
                   , scopePublic  :: Names
                   , scopePrivate :: Names
                   } deriving (Show)

newtype Visibility = V { unV :: Lens' Scope Names }

publicVisibility, privateVisibility :: Visibility

publicVisibility = V $ \ f Scope { .. } ->
  fmap (\ ns -> Scope { scopePublic = ns, .. }) (f scopePublic)

privateVisibility = V $ \ f Scope { .. } ->
  fmap (\ ns -> Scope { scopePrivate = ns, .. }) (f scopePrivate)

emptyScope :: Namespace -> [T.Text] -> Scope
emptyScope scopeNS scopePrefix =
  Scope { scopePublic  = mempty
        , scopePrivate = mempty
        , .. }

topScope :: [T.Text] -> Scope
topScope pfx = emptyScope (packNamespace pfx) pfx

localScope :: Namespace -> [T.Text] -> Scope
localScope outer pfx = emptyScope (outer `dot` packNamespace pfx) pfx

declScope :: Scope -> Scope
declScope parent = emptyScope (scopeNS parent) (scopePrefix parent)

mergeScope :: Scope -> [Scope] -> [Scope]
mergeScope _ []              = [] -- XXX: is this just an error?
mergeScope s (parent : rest) =
  parent { scopePublic = qualify (scopePrefix s) (scopePublic s)
                         `mappend` scopePublic parent
         } : rest

-- INVARIANT: rwContext should never be empty
data RW = RW { rwContext    :: [Scope]
             , rwVisibility :: Visibility
             }

pushScope :: Scope -> RW -> RW
pushScope scope rw = rw { rwContext = scope : rwContext rw }

popScope :: RW -> RW
popScope rw =
  case rwContext rw of
    scope : rest -> rw { rwContext = mergeScope scope rest }
    _            -> rw

currentScope :: Lens' RW Scope
currentScope f rw =
  case rwContext rw of
    scope : rest -> fmap (\ scope' -> rw { rwContext = scope' : rest }) (f scope)
    _            -> panic (text "Scope stack underflow")


newtype RN a = RN { unRN :: StateT RW Dang a
                  } deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)
  {-# INLINE inBase #-}

rename :: [T.Text] -> RN a -> Dang a
rename ns (RN m) =
  do (a,_) <- runM m RW { rwContext = [topScope ns]
                        , rwVisibility = publicVisibility }
     return a

-- | Enter a module, and push a new scope on the context stack.
withModuleScope :: IdentOf Parsed -> (Name -> RN a) -> RN a
withModuleScope lpname body =
  do n  <- newModuleBind lpname
     ns <- currentNamespace
     RN (sets_ (pushScope (localScope ns (pnameNamespace lpname))))
     a  <- body n
     RN (sets_ popScope)
     return a

-- | A local scope, introduced by a declaration.
withDeclScope :: RN a -> RN a
withDeclScope body =
  do RN (sets_ (\ rw -> pushScope (declScope (view currentScope rw)) rw))
     a <- body
     RN (sets_ popScope)
     return a


-- | The global namespace of the current scope.
currentNamespace :: RN Namespace
currentNamespace  = RN $
  do RW { .. } <- get
     case rwContext of
       scope : _ -> return (scopeNS scope)
       _         -> panic (text "Scope stack underflowed")


-- | Set visibility.
withVisibility :: Visibility -> RN a -> RN a
withVisibility vis (RN m) = RN $
  do old <- sets (\ rw -> ( rwVisibility rw, rw { rwVisibility = vis }))
     a   <- m
     sets_ (\ rw -> rw { rwVisibility = old })
     return a

-- | Get visibility.
currentVisibility :: RN Visibility
currentVisibility  = RN $
  do RW { rwVisibility = vis } <- get
     return vis


-- | Make a new module name, for a module within a compilation unit.
--
-- INVARIANT: this should never be a qualified identifier, as it's only possible
-- to give an unqualified name in the parsed syntax.
newMod :: IdentOf Parsed -> RN Name
newMod lpname =
  do ns <- currentNamespace
     RN (withSupply (mkModName (Just ns) (expectUnqual lpname) (range lpname)))

-- | Make a new binding name.
--
-- INVARIANT: this should never be a qualified identifier, as it's only possible
-- to give an unqualified name in the parsed syntax.
newBind :: IdentOf Parsed -> RN Name
newBind lpname =
  do ns <- currentNamespace
     RN (withSupply (mkBinding ns (expectUnqual lpname) (range lpname)))

-- | Make a new name for a parameter.
--
-- INVARIANT: this should never be a qualified identifier.
newParam :: ParamSource -> IdentOf Parsed -> RN Name
newParam src lpname =
  RN (withSupply (mkParam src (expectUnqual lpname) (range lpname)))


addPName :: (T.Text -> Def) -> Visibility -> IdentOf Parsed -> Name -> RN ()
addPName mkDef vis lpname n =
  RN (sets_ (over (currentScope . unV vis) (insertPName mkDef lpname [n])))

addValue, addMod, addType :: Visibility -> IdentOf Parsed -> Name -> RN ()
addValue = addPName DefVal
addMod   = addPName DefMod
addType  = addPName DefType


-- | Introduce a value-level name.
newValueBind :: IdentOf Parsed -> RN (IdentOf Renamed)
newValueBind lpname =
  do n   <- newBind lpname
     vis <- currentVisibility
     addValue vis lpname n
     return n

-- | Introduce a value-level parameter.
newValueParam :: ParamSource -> IdentOf Parsed -> RN (IdentOf Renamed)
newValueParam parent lpname =
  do n <- newParam parent lpname
     addValue privateVisibility lpname n
     return n

-- | Introduce a module name.
newModuleBind :: IdentOf Parsed -> RN (IdentOf Renamed)
newModuleBind lpname =
  do n   <- newMod lpname
     vis <- currentVisibility
     addMod vis lpname n
     return n

-- | Introduce a module parameter.
newFunctorParam :: ParamSource -> IdentOf Parsed -> RN (IdentOf Renamed)
newFunctorParam parent lpname =
  do n <- newParam parent lpname
     addMod privateVisibility lpname n
     return n

-- | Introduce a type-level name.
newTypeBind :: IdentOf Parsed -> RN (IdentOf Renamed)
newTypeBind lpname =
  do n   <- newBind lpname
     vis <- currentVisibility
     addType vis lpname n
     return n

-- | Introduce a type-level parameter.
newTypeParam :: ParamSource -> IdentOf Parsed -> RN (IdentOf Renamed)
newTypeParam parent lpname =
  do n <- newParam parent lpname
     addType privateVisibility lpname n
     return n


-- | Panic if a qualified name is given.
expectUnqual :: PName -> T.Text
expectUnqual (PUnqual _ n) = n
expectUnqual (PQual _ _ _) = panic (text "Expected an unqualified name")


-- Renaming --------------------------------------------------------------------

type Rename f = f Parsed -> RN (f Renamed)

-- | Special renaming for the top-most module.
rnTopModule :: Rename Module
rnTopModule m =
  withLoc (modMeta m) $
    -- NOTE: don't actually record the name in the environment, as we're
    -- currently inside this module.
    do n' <- newMod (modName m)
       rnModuleAux n' m

rnModule :: Rename Module
rnModule m =
  withLoc (modMeta m) $
  withModuleScope (modName m) $ \ n' ->
       rnModuleAux n' m

rnModuleAux :: Name -> Rename Module
rnModuleAux n' Module { .. } =
  do bs' <- traverse rnDecl modDecls
     return Module { modName     = n'
                   , modMeta     = modMeta
                   , modRequires = []
                   , modDecls    = bs'
                   }

rnDecl :: Rename Decl

rnDecl (DBind loc bind) = withLoc loc (DBind loc <$> rnBind bind)

rnDecl (DSig _ sig) =
  panic $ text "Unexpected DSig remaining, bug in resolveSignatures?" $$
          pp (sigName sig)

rnDecl (DData loc dta) = withLoc loc (DData loc <$> rnData dta)

rnDecl (DSyn loc syn) = withLoc loc (DSyn loc <$> rnSyn syn)

rnDecl (DModBind loc lname e) =
  withLoc loc $
  withModuleScope lname $ \ name ->
    DModBind loc name <$> rnModExpr (Just (FromMod name)) e

rnDecl (DModType loc lname ty) =
  withLoc loc $
  withModuleScope lname $ \ name ->
    DModType loc name <$> rnModType ty


rnModExpr :: Maybe ParamSource -> Rename ModExpr
rnModExpr mbParent = go
  where
  go (MEName loc n) = withLoc loc $
    MEName loc <$> rnModName n

  go (MEApp loc f x) = withLoc loc $
    MEApp loc <$> go f <*> go x

  go (MEStruct loc s) = withLoc loc (MEStruct loc <$> rnModStruct s)

  go (MEFunctor loc p ty e) = withLoc loc $
    do p'  <- newFunctorParam (fromMaybe (FromFunctor loc) mbParent) p
       ty' <- rnModType ty
       e'  <- go e
       return (MEFunctor loc p' ty' e')

  go (MEConstraint loc m ty) = withLoc loc $
    MEConstraint loc <$> go m <*> rnModType ty


-- | Rename the declarations held within a struct.
rnModStruct :: Rename ModStruct
rnModStruct (ModStruct loc es) = withLoc loc $
  ModStruct loc <$> traverse rnDecl es


rnModType :: Rename ModType

rnModType (MTVar loc n) =
  MTVar loc <$> rnModName n

rnModType (MTSig loc sig) = withLoc loc $
  MTSig loc <$> traverse rnModSpec sig

rnModType (MTFunctor loc p ty rty) = undefined


rnModSpec :: Rename ModSpec
rnModSpec (MSSig  loc sig) = withLoc loc (MSKind loc <$> rnSig sig)
rnModSpec (MSKind loc sig) = withLoc loc (MSKind loc <$> rnSig sig)
rnModSpec (MSData loc dat) = withLoc loc (MSData loc <$> rnData dat)
rnModSpec (MSMod  loc n ty) = withLoc loc $
  MSMod loc <$> newModuleBind n <*> rnModType ty


-- | Introduce names from a binding.
introBind :: Bind Parsed -> RN ()
introBind Bind { .. } =
  do _ <- withLoc bMeta (newValueBind bName)
     return ()

-- | Rename a binding.
rnBind :: Rename Bind
rnBind b =
  do introBind b
     rnBindAux b

-- | Rename a binding, assuming that it's name has already been introduced.
rnBindAux :: Rename Bind
rnBindAux Bind { .. } =
  withLoc bMeta $
  withDeclScope $
    do n'  <- rnValueName bName
       ps' <- traverse (rnPat (FromBind n')) bParams
       b'  <- rnExpr bBody
       ty' <- traverse (rnSchema (FromBind n')) bSig
       return Bind { bName = n'
                   , bParams = ps'
                   , bBody = b'
                   , bSig = ty'
                   , .. }

-- | Rename a value signature from a module type. This introduces a fresh name
-- for the name bound by the signature, as there is no accompanying value
-- binding at this point.
rnSig :: Rename Sig
rnSig Sig { .. } = withLoc sigMeta $
  do name <- newValueBind sigName
     withDeclScope $
       do ty <- rnSchema (FromType name) sigSchema
          return Sig { sigName = name, sigSchema = ty, .. }


-- | Introduce names for the type that is introduced, as well as for each
-- constructor. Rename type parameters for each constructor.
rnData :: Rename Data
rnData Data { .. } = withLoc dMeta $
  do ty <- newTypeBind dName
     ps <- mapM (newTypeParam (FromType ty)) dParams
     cs <- mapM rnConstr dConstrs
     return Data { dName = ty
                 , dParams = ps
                 , dConstrs = cs
                 , .. }


rnConstr :: Rename Constr
rnConstr Constr { .. } = withLoc cMeta $
  do name <- newValueBind cName
     ps   <- mapM rnType cParams
     return Constr { cName = name, cParams = ps, .. }


-- | Rename a type synonym.
rnSyn :: Rename Syn
rnSyn Syn { .. } = withLoc synMeta $
  do name' <- newTypeBind synName
     ps'   <- mapM (newTypeParam (FromType name')) synParams
     ty'   <- rnType synType
     return Syn { synName = name', synParams = ps', synType = ty', .. }


rnSchema :: ParamSource -> Rename Schema
rnSchema src (Schema loc ps ty) = withLoc loc $
  Schema loc <$> traverse (newTypeParam src) ps
             <*> rnType ty


rnType :: Rename Type
rnType (TCon loc n) = withLoc loc $
     TCon loc <$> rnTypeName n

rnType (TVar loc v) = withLoc loc $
     TVar loc <$> rnTypeName v

rnType (TApp loc f ps) = withLoc loc $
     TApp loc <$> rnType f <*> traverse rnType ps

rnType (TFun loc a b) = withLoc loc $
     TFun loc <$> rnType a <*> rnType b


rnPat :: ParamSource -> Rename Pat

rnPat parent (PVar loc v) =
  withLoc loc (PVar loc <$> newValueParam parent v)

rnPat _ (PWild loc) =
  return (PWild loc)

rnPat parent (PCon loc con ps) =
  withLoc loc (PCon loc <$> rnValueName con <*> traverse (rnPat parent) ps)

rnExpr :: Rename Expr
rnExpr (EVar loc n)     = withLoc loc (EVar loc <$> rnValueName n)
rnExpr (ECon loc n)     = withLoc loc (ECon loc <$> rnValueName n)
rnExpr (EApp loc f xs)  = withLoc loc (EApp loc <$> rnExpr f <*> traverse rnExpr xs)

rnExpr (EAbs loc xs m)  = withLoc loc $
  EAbs loc <$> traverse (rnPat (FromLambda loc)) xs <*> rnExpr m

rnExpr (ELit loc lit)   = withLoc loc (ELit loc <$> rnLit lit)
rnExpr (ELet loc lds e) =
  withLoc loc $
  rnLetDecls lds $ \ lds' ->
    ELet loc lds' <$> rnExpr e

rnExpr (ECase loc e m) =
  withLoc loc $
    ECase loc <$> rnExpr e <*> rnMatch (FromCase loc) m


rnMatch :: ParamSource -> Rename Match
rnMatch parent = go
  where
  go (MPat loc p m) = withLoc loc $
    MPat loc <$> rnPat parent p <*> go m

  go (MSplit loc l r) = withLoc loc $
    MSplit loc <$> go l <*> go r

  go (MFail loc) =
    pure (MFail loc)

  go (MExpr loc e) = withLoc loc $
    MExpr loc <$> rnExpr e

-- | Introduce names for all declarations in the block, then rename each
-- declaration.
rnLetDecls :: [LetDecl Parsed] -> ([LetDecl Renamed] -> RN a) -> RN a
rnLetDecls lds body =
  do introBinds lds
     lds' <- traverse rnLetDecl lds
     body lds'

  where

  introBinds [] = return ()

  introBinds (LDBind _ b : rest) =
    do introBind b
       introBinds rest

  -- signatures don't introduce bindings
  introBinds (LDSig _ _ : rest) =
       introBinds rest


-- | Rename a single let declaration.
--
-- NOTE: binding names will be introduced by the let block, as let is recursive.
rnLetDecl :: Rename LetDecl
rnLetDecl (LDBind loc b) = withLoc loc (LDBind loc <$> rnBindAux b)
rnLetDecl (LDSig  loc s) =
  panic $ text "Unexpected LDSig remaining, bug in resolveSignatures?" $$
          pp (sigName s)


rnLit :: Rename Literal
rnLit (LInt loc a b) = pure (LInt loc a b)


-- | Find the canonical name for this value-level variable.
rnValueName :: IdentOf Parsed -> RN (IdentOf Renamed)
rnValueName  = rnPName DefVal

rnTypeName :: IdentOf Parsed -> RN (IdentOf Renamed)
rnTypeName  = rnPName DefType

-- | Find the canonical name of this module name.
rnModName :: HasCallStack => IdentOf Parsed -> RN (IdentOf Renamed)
rnModName  = rnPName DefMod


rnPName :: (T.Text -> Def) -> IdentOf Parsed -> RN (IdentOf Renamed)
rnPName mkDef lpname =
  do RW { rwContext = ctx } <- RN get
     case resolve ctx of
       Just [] -> panic (text "Malformed name scope")
       Just ns -> return (head ns)
       Nothing -> missingBinding lpname

  where

  resolve (scope:rest) = check (scopePrivate scope)
                       $ check (scopePublic scope)
                       $ go rest

  resolve [] = panic (text "Invalid scope context")

  check names tryOther =
    do NameNode mb _ <- lookupPName mkDef lpname names
       mb
    <|>
    tryOther

  go []             = Nothing
  go (scope : rest) = check (scopePublic scope) (go rest)


-- Errors ----------------------------------------------------------------------

missingBinding :: IdentOf Parsed -> RN (IdentOf Renamed)
missingBinding lpname = withLoc lpname $
  do addError ErrRnUnknown (pp lpname)
     ns  <- currentNamespace
     loc <- askLoc
     RN (withSupply (mkUnknown (Declaration (ModInfo ns)) lpname loc))
