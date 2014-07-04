{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.ScopeCheck where

import Dang.ModuleSystem.Interface
import Dang.ModuleSystem.QualName
import Dang.Monad
import Dang.Syntax.AST
import Dang.Utils.Location
import Dang.Utils.Panic ( panic )
import Dang.Utils.Pretty

import           Control.Applicative ( Applicative )
import           Control.Lens ( view, set )
import           Data.Foldable ( foldMap )
import           Data.Generics ( Data, gmapM, extM, ext1M )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import           MonadLib ( BaseM(..), runM, Id, ReaderT, ask, local )


-- | Fully-qualify all names in the module, according to their imports.
scopeCheckModule :: Module -> Dang Module
scopeCheckModule m = failErrs (runScope (scModule m))

scPanic :: Pretty msg => msg -> a
scPanic  = panic "Dang.ModuleSystem.ScopeCheck"


-- Scope Checking Monad --------------------------------------------------------

newtype Scope a = Scope { unScope :: ReaderT Names Dang a
                        } deriving (Functor,Applicative,Monad)

instance BaseM Scope Dang where
  inBase m = Scope (inBase m)

runScope :: Scope a -> Dang a
runScope m = runM (unScope m) mempty

withNames :: Names -> Scope a -> Scope a
withNames names m = Scope $
  do env <- ask
     local (names `shadowing` env) (unScope m)

resolveName :: QualName -> Scope QualName
resolveName qn = Scope $
  do env <- ask
     case Map.lookup qn (nDefs env) of

       Just [def] -> return (defName def)

       Just []    -> scPanic (text "Invalid naming environment")

       Just defs  -> do addErr (multipleDefinitions defs)
                        return qn

       Nothing    -> do addErr (missingSymbol qn)
                        return qn

-- | The error text for multiple uses of the same name.
multipleDefinitions :: [NameDef] -> PPDoc
multipleDefinitions defs = undefined

-- | The error text for a missing symbol.
missingSymbol :: QualName -> PPDoc
missingSymbol qn = text "Not in scope: " <+> quoted (pp qn)


-- Renaming Environments -------------------------------------------------------

data NameDef = FromDef (Located QualName)
               -- ^ The location of the definition
             | FromIface (Located Open) QualName
               -- ^ The location of the import responsible
               deriving (Show)

-- | The real name associated with a NameDef.
defName :: NameDef -> QualName
defName (FromDef Located { .. }) = locValue
defName (FromIface _ qn)         = qn



newtype Names = Names { nDefs :: Map.Map QualName [NameDef]
                      } deriving (Show)

instance Monoid Names where
  {-# INLINE mempty #-}
  mempty      = Names { nDefs = Map.empty }

  {-# INLINE mappend #-}
  mappend l r = mconcat [l,r]

  {-# INLINE mconcat #-}
  mconcat ns  = Names { nDefs = Map.unionsWith (++) (map nDefs ns) }


-- | Combine two naming environments, where the names from the left shadow the
-- ones on the right.
shadowing :: Names -> Names -> Names
l `shadowing` r = Names { nDefs = Map.union (nDefs l) (nDefs r) }

-- | Generate a naming environment from a declaration.  From the module:
--
-- > module A where
-- > f x = 10
--
-- The naming environment can be generated from this call:
--
-- > fromDef [| f |] [| A.f |]
--
fromDef :: QualName -> Located QualName -> Names
fromDef n ln = Names { nDefs = Map.singleton n [FromDef ln] }

-- | Generate a naming environment from an import declaration.  From the
-- renaming import of the module A that defines f:
--
-- > open A as A'
--
-- The enviroment would be generated like this:
--
-- > fromImport [| A'.f |] [| open A as A' |] [| A.f |]
--
fromImport :: QualName -> Located Open -> QualName -> Names
fromImport n lo qn = Names { nDefs = Map.singleton n [FromIface lo qn] }


-- Scope Checking --------------------------------------------------------------

-- | Generic traversal.
scPass :: Data a => a -> Scope a
scPass  = gmapM scPass
  `ext1M` scLocated
   `extM` scName

-- | Rename a module.
scModule :: Module -> Scope Module
scModule m =
  do let names = modNames m
     logInfo (text (show names))
     withNames names (scPass m)

-- | Annotate errors with the current location
scLocated :: Data a => Located a -> Scope (Located a)
scLocated Located { .. } = withLoc locRange $
  do a <- scPass locValue
     return Located { locValue = a, .. }

-- | Resolve names.
scName :: Name -> Scope Name
scName n =
  do qn' <- resolveName (view qualName n)
     return (set qualName qn' n)


-- Name Gathering --------------------------------------------------------------

-- | Generate the map from unqualified to qualified names that a module
-- generates.
modNames :: Module -> Names
modNames m = foldMap (topDeclNames namespace) (modDecls m)
  where
  namespace = unLoc (modName m)

topDeclNames :: ModName -> TopDecl -> Names
topDeclNames ns (TDDecl d)      = declNames ns d
topDeclNames ns (TDData d)      = foldMap (dataNames ns) d
topDeclNames ns (TDPrimType pt) = primTypeNames ns pt
topDeclNames ns (TDPrimTerm pt) = primTermNames ns pt
topDeclNames ns (TDLocal ld)    = foldMap (localNames    ns) ld
topDeclNames ns (TDExport e)    = foldMap (foldMap (foldMap (topDeclNames ns))) e

declNames :: ModName -> Decl -> Names
declNames ns d = mempty

dataNames :: ModName -> DataDecl -> Names
dataNames ns d = mempty

primTypeNames :: ModName -> Located PrimType -> Names
primTypeNames ns lpt = fromDef qname (lpt {locValue = resolved })
  where
  qname    = view qualName (primTypeName (unLoc lpt))
  sym      = view qualSymbol qname
  resolved = Qual (Type 0) ns sym

primTermNames :: ModName -> Located PrimTerm -> Names
primTermNames ns lpt = fromDef qname (lpt { locValue = resolved })
  where
  qname    = view qualName (primTermName (unLoc lpt))
  sym      = view qualSymbol qname
  resolved = Qual Expr ns sym

localNames :: ModName -> LocalDecls -> Names
localNames ns ld = mempty
