{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Dang.Unique (SupplyM,withSupply)
import Dang.Utils.Ident (Namespace,packNamespaceLazy,dot)
import Dang.Utils.PP
import Dang.Utils.Panic

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import           Control.Lens (Lens',over,view)
import qualified Data.Foldable as F
import           Data.List (nub,partition)
import           Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),StateT,get,sets,sets_)

import Debug.Trace


data Renamed

type instance IdentOf  Renamed = Name
type instance TypeOf   Renamed = Type Renamed
type instance SchemaOf Renamed = Schema Renamed
type instance MetaOf   Renamed = SrcRange


-- | Rename a top-level module.
renameModule :: HasCallStack => Module Parsed -> Dang (Module Renamed)
renameModule m =
  rename (pnameNamespace (thing (modName m))) (rnTopModule m)


-- Renaming Monad --------------------------------------------------------------

type Names = NameTrie [Name]

data Scope = Scope { scopeNS      :: !Namespace -- ^ Fully-qualified namespace
                   , scopePrefix  :: [L.Text]   -- ^ Relative prefix
                   , scopePublic  :: Names
                   , scopePrivate :: Names
                   } deriving (Show)

newtype Visibility = V { unV :: Lens' Scope Names }

publicVisibility, privateVisibility :: Visibility

publicVisibility = V $ \ f Scope { .. } ->
  fmap (\ ns -> Scope { scopePublic = ns, .. }) (f scopePublic)

privateVisibility = V $ \ f Scope { .. } ->
  fmap (\ ns -> Scope { scopePrivate = ns, .. }) (f scopePrivate)

emptyScope :: Namespace -> [L.Text] -> Scope
emptyScope scopeNS scopePrefix =
  Scope { scopePublic  = mempty
        , scopePrivate = mempty
        , .. }

topScope :: [L.Text] -> Scope
topScope pfx = emptyScope (packNamespaceLazy pfx) pfx

localScope :: Namespace -> [L.Text] -> Scope
localScope outer pfx = emptyScope (outer `dot` packNamespaceLazy pfx) pfx

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

rename :: [L.Text] -> RN a -> Dang a
rename ns (RN m) =
  do (a,_) <- runM m RW { rwContext = [topScope ns]
                        , rwVisibility = publicVisibility }
     return a

-- | Enter a module, and push a new scope on the context stack.
withModuleScope :: IdentOf Parsed -> (Name -> RN a) -> RN a
withModuleScope lpname body =
  do n  <- newModuleBind lpname
     ns <- currentNamespace
     RN (sets_ (pushScope (localScope ns (pnameNamespace (thing lpname)))))
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
     RN (withSupply (mkModName (Just ns) (expectUnqual (thing lpname)) (getLoc lpname)))

-- | Make a new binding name.
--
-- INVARIANT: this should never be a qualified identifier, as it's only possible
-- to give an unqualified name in the parsed syntax.
newBind :: IdentOf Parsed -> RN Name
newBind lpname =
  do ns <- currentNamespace
     RN (withSupply (mkBinding ns (expectUnqual (thing lpname)) (getLoc lpname)))

-- | Make a new name for a parameter.
--
-- INVARIANT: this shold never be a qualified identifier.
newParam :: ParamSource -> IdentOf Parsed -> RN Name
newParam src lpname =
  RN (withSupply (mkParam src (expectUnqual (thing lpname)) (getLoc lpname)))


addPName :: (L.Text -> Def) -> Visibility -> IdentOf Parsed -> Name -> RN ()
addPName mkDef vis lpname n =
  RN (sets_ (over (currentScope . unV vis) (insertPName mkDef (thing lpname) [n])))

addValue, addMod, addType :: Visibility -> IdentOf Parsed -> Name -> RN ()
addValue = addPName DefDecl
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
newValueParam :: IdentOf Renamed -> IdentOf Parsed -> RN (IdentOf Renamed)
newValueParam parent lpname =
  do n <- newParam (FromBind parent) lpname
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
newFunctorParam :: IdentOf Renamed -> IdentOf Parsed -> RN (IdentOf Renamed)
newFunctorParam parent lpname =
  do n <- newParam (FromFunctor parent) lpname
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
newTypeParam :: IdentOf Renamed -> IdentOf Parsed -> RN (IdentOf Renamed)
newTypeParam parent lpname =
  do n <- newParam (FromSig parent) lpname
     addType privateVisibility lpname n
     return n


expectUnqual :: PName -> L.Text
expectUnqual (PUnqual n) = n
expectUnqual (PQual _ _) = panic (text "Expected an unqualified name")


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
rnDecl (DSig  loc sig)  = withLoc loc (DSig  loc <$> rnSig  sig)
rnDecl (DData loc dta)  = withLoc loc (DData loc <$> rnData dta)

rnDecl (DModBind loc lname mexpr) = undefined
rnDecl (DModType loc lname mtype) = undefined


-- | Introduce names from a binding.
introBind :: Bind Parsed -> RN ()
introBind Bind { .. } body =
  do _ <- withLoc bMeta (newValueBind bName)
     return ()


rnBind :: Rename Bind
rnBind b =
  withLoc (bMeta b) $
  do introBind b
     withDeclScope $
       do n'  <- rnValueName (bName b)
          ps' <- traverse rnPat (bParams b)
          b'  <- rnExpr (bBody b)
          return Bind { bName = n', bMeta = bMeta b, bParams = ps', bBody = b' }

-- | Rename a binding, assuming that it's name has already been introduced.
rnBindAux :: Rename Bind
rnBindAux Bind { .. } =
  do n'  <- rnValueName (bName b)
     ps' <- traverse rnPat (bParams b)
     b'  <- rnExpr (bBody b)
     return Bind { bName = n', bMeta = bMeta b, bParams = ps', bBody = b' }

rnSig :: Rename Sig
rnSig  = undefined

rnData :: Rename Data
rnData  = undefined

rnPat :: Rename Pat
rnPat  = undefined

rnExpr :: Rename Expr
rnExpr (EVar loc n)     = withLoc loc (EVar loc <$> rnValueName n)
rnExpr (ECon loc n)     = withLoc loc (ECon loc <$> rnValueName n)
rnExpr (EApp loc f xs)  = withLoc loc (EApp loc <$> rnExpr f <*> traverse rnExpr xs)
rnExpr (EAbs loc m)     = undefined
rnExpr (ELit loc lit)   = withLoc loc (ELit loc <$> rnLit lit)
rnExpr (ELet loc lds e) =
  withLoc loc $
  rnLetDecls lds $ \ lds' ->
    ELet loc lds' <$> rnExpr e


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


rnLetDecl :: Rename LetDecl
rnLetDecl (LDBind loc b) = withLoc loc (rnBindAux b)




rnLit :: Rename Literal
rnLit (LInt loc a b) = pure (LInt loc a b)


-- | Find the canonical name for this value-level variable.
rnValueName :: IdentOf Parsed -> RN (IdentOf Renamed)
rnValueName  = rnPName DefDecl

-- | Find the canonical name of this module name.
rnModName :: HasCallStack => IdentOf Parsed -> RN (IdentOf Renamed)
rnModName  = rnPName DefMod


rnPName :: (L.Text -> Def) -> IdentOf Parsed -> RN (IdentOf Renamed)
rnPName mkDef lpname =
  do RW { rwContext = ctx } <- RN get
     io (print ctx)
     case resolve ctx of
       Just ns -> return (head ns)
       Just [] -> panic (text "Malformed name scope")
       Nothing -> missingBinding lpname

  where

  resolve (scope:rest) = check (scopePrivate scope)
                       $ check (scopePublic scope)
                       $ go rest

  check names tryOther =
    do NameNode mb _ <- lookupPName mkDef (thing lpname) names
       mb
    <|>
    tryOther

  go []             = Nothing
  go (scope : rest) = check (scopePublic scope) (go rest)


-- Errors ----------------------------------------------------------------------

missingBinding :: IdentOf Parsed -> RN (IdentOf Renamed)
missingBinding lpname =
  do io (print ("missing", lpname))
     withLoc lpname (addError ErrRnUnknown (pp (thing lpname)))
     -- XXX invent an unknown name
     undefined
