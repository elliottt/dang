{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.ScopeCheck where

import Dang.ModuleSystem.Interface
import Dang.ModuleSystem.QualName
import Dang.ModuleSystem.Types ( Exported(..) )
import Dang.Monad
import Dang.Syntax.AST
import Dang.Utils.Location
import Dang.Utils.Panic ( panic )
import Dang.Utils.Pretty

import           Control.Applicative ( Applicative )
import           Control.Lens as Lens ( view, set, traverseOf )
import           Data.Foldable ( foldMap )
import           Data.Generics ( Data, gmapM, extM, ext1M )
import qualified Data.Map.Strict as Map
import           Data.Monoid ( Monoid(..) )
import qualified Data.Set as Set
import           Data.Traversable ( traverse )
import           MonadLib
                     ( BaseM(..), runM, Id, ReaderT, ask, local, StateT, get
                     , set )


-- | Fully-qualify all names in the module, according to their imports.
scopeCheckModule :: Module -> Dang Module
scopeCheckModule m = failErrs (runScope (scModule m))

scPanic :: Pretty msg => msg -> a
scPanic  = panic "Dang.ModuleSystem.ScopeCheck"


-- Scope Checking Monad --------------------------------------------------------

data RW = RW { rwNext :: Map.Map String Int
             }

data RO = RO { roNames   :: Names
             , roModName :: ModName
             }

newtype Scope a = Scope { unScope :: ReaderT RO (StateT RW Dang) a
                        } deriving (Functor,Applicative,Monad)

instance BaseM Scope Dang where
  inBase m = Scope (inBase m)

runScope :: Scope a -> Dang a
runScope m =
  do (a,_) <- runM (unScope m) RO { roNames = mempty, roModName = [] }
                               RW { rwNext  = mempty }
     return a


-- | Locally override the current namespace used when qualifying names.
withModName :: ModName -> Scope a -> Scope a
withModName ns m = Scope $
  do ro <- ask
     local ro { roModName = ns } (unScope m)

getModName :: Scope ModName
getModName  = Scope (roModName `fmap` ask)

-- | Introduce a group of names in a scope, shadowing existing names that would
-- have overlapped.
--
-- XXX this needs to emit warnings when shadowing occurs.
withNames :: Names -> Scope a -> Scope a
withNames names m =
  do names' <- freshen names
     logInfo (text (show names'))
     Scope $ do ro <- ask
                local ro { roNames = names' `shadowing` roNames ro } (unScope m)

getNames :: Scope Names
getNames  = Scope (roNames `fmap` ask)

-- | Resolve a single name to whatever is in scope at that level.  There are
-- three error cases:
--
--  1. The name isn't present in the current scope
--  2. Multiple bindings to the same name are in scope
--  3. (panic) The name is known, but no names are present
resolveName :: QualName -> Scope QualName
resolveName qn =
  do env <- getNames
     case Map.lookup qn (nDefs env) of

       Just [def] -> return (defName def)

       Just []    -> scPanic (text "Invalid naming environment")

       Just defs  -> do addErr (multipleDefinitions qn defs)
                        return qn

       Nothing    -> do addErr (missingSymbol qn)
                        return qn

-- | The error text for multiple uses of the same name.
multipleDefinitions :: QualName -> [NameDef] -> PPDoc
multipleDefinitions qn defs =
  hang (text "multiple definitions for " <+> quoted (pp qn))
     2 (vcat (map pp defs))

-- | The error text for a missing symbol.
missingSymbol :: QualName -> PPDoc
missingSymbol qn = text "Not in scope:" <+> quoted (pp qn)


-- Renaming Environments -------------------------------------------------------

data NameDef = FromBind (Located QualName)
               -- ^ The location of the definition
             | FromParam (Located QualName)
               -- ^ The location of the parameter
             | FromIface (Located Open) QualName
               -- ^ The location of the import responsible
               deriving (Show)

instance Pretty NameDef where
  ppr (FromBind  lq)   = text "declaration:" <+> ppWithLoc lq
  ppr (FromParam lq)   = text "parameter:"   <+> ppWithLoc lq
  ppr (FromIface lo _) = text "imported:"    <+> ppWithLoc lo

-- | The real name associated with a NameDef.
defName :: NameDef -> QualName
defName (FromBind Located { .. })   = locValue
defName (FromParam Located { .. }) = locValue
defName (FromIface _ qn)           = qn



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

-- | Qualify the domain of a name mapping, turning all unqualified name mappings
-- into qualified ones.
qualify :: ModName -> Names -> Names
qualify ns Names { .. } =
  Names { nDefs = Map.mapKeys update nDefs }
  where
  update (Qual l [] sym) = Qual l ns sym
  update q@Qual{}        = q
  update (Param l sym)   = Qual l ns sym

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
fromDef n ln = Names { nDefs = Map.singleton n [FromBind ln] }

-- | Generate a naming environment from a parameter.  From the declaration
--
-- > f x = ...
--
-- the naming environment for `x` can be generated from this call:
--
-- > fromParam [| x |]
--
-- `x` is assumed to have no namespace associated with it.
fromParam :: SrcLoc -> Name -> Names
fromParam loc n = Names { nDefs = Map.singleton qn [FromParam (p `at` loc)] }
  where
  qn = view qualName n
  p  = Param (view qualLevel qn) (view qualSymbol qn)

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


-- | Pick fresh variations of the names present in the range of the name map.
freshen :: Names -> Scope Names
freshen ns =
  do ns' <- Map.traverseWithKey freshenDef (nDefs ns)
     return (Names ns')


freshenDef :: QualName -> [NameDef] -> Scope [NameDef]

-- only freshen qualified names from this module
freshenDef qn defs@[FromBind lqn] =
  do ns <- getModName
     if view qualModule (locValue lqn) == ns
        then do lqn' <- traverse freshenSym lqn
                return [FromBind lqn']
        else return defs

-- Always freshen params
freshenDef qn [FromParam lqn] =
  do lqn' <- traverse freshenSym lqn
     return [FromParam lqn']

freshenDef _ defs =
     return defs


freshenSym :: QualName -> Scope QualName
freshenSym  = traverseOf qualSymbol $ \ sym -> Scope $
  do rw <- get
     let (sym',rwNext') = findNext sym (rwNext rw)
     MonadLib.set $! rw { rwNext = rwNext' }
     return sym'
  where
  findNext sym ixs =
    case Map.lookup sym ixs of
      Just n  -> checkDone (sym ++ show n) (Map.insert sym (n + 1) ixs)
      Nothing -> (sym, Map.insert sym 0 ixs)

  checkDone sym ixs =
    case Map.lookup sym ixs of
      Just{}  -> findNext sym ixs
      Nothing -> (sym,ixs)


-- Scope Checking --------------------------------------------------------------

-- | Generic traversal.
scPass :: Data a => a -> Scope a
scPass  = gmapM scPass
  `ext1M` scLocated
   `extM` scLocalDecls
   `extM` scBind
   `extM` scMatch
   `extM` scName


-- | Rename a module.
scModule :: Module -> Scope Module
scModule m = withModName (unLoc (modName m)) $
  do names <- fullNames (modNames m)
     -- XXX import the environment described by the Open declarations
     withNames names (scPass m)


-- | Annotate errors and warnings with the current location
scLocated :: Data a => Located a -> Scope (Located a)
scLocated Located { .. } = withLoc locRange $
  do a <- scPass locValue
     return Located { locValue = a, .. }


-- | Value bindings.  This makes the assumption that the name of the binding has
-- been freshened by its enclosing scope.  This introduces a few cases:
--
--  1. The binding is a top-level value in a module, and thus freshened by the
--     path from scModule
--
--  2. The binding is from a `local .. in ..` declaration, in which case it is
--     freshened by scLocalDecls.
--
--  3. The binding is from a `let .. in ..` expression, in which case it is
--     freshened by the call to scExpr
--
--  In any case, the binding name is simply renamed locally, either qualifying
--  it the public cases, or freshening it for the private cases.
scBind :: Bind -> Scope Bind
scBind Bind { .. } = 
  do bindName <- scPass bindName
     bindType <- scPass bindType
     bindBody <- scPass bindBody
     return Bind { .. }


-- | Rename a group of local declarations.  Only the local declarations are
-- brought into scope, as the exported declarations will be already named by the
-- enclosing naming environment.  See localNames.
scLocalDecls :: LocalDecls -> Scope LocalDecls
scLocalDecls LocalDecls { .. } =
  do ns     <- getModName
     lNames <- fullNames (foldMap (declNames ns) ldLocals)
     withNames lNames $
       do ldLocals <- scPass ldLocals
          ldDecls <- scPass ldDecls
          return LocalDecls { .. }


-- | Introduce the names bound by the pattern, when checking the body of the
-- Match.
scMatch :: Match -> Scope Match

scMatch (MPat p m) =
  do loc   <- askLoc
     names <- fullNames (patNames loc p)
     withNames names $
       do p'    <- scPass p
          m'    <- scMatch m
          return (MPat p' m')

scMatch (MGuard p e m) =
  do loc   <- askLoc
     names <- fullNames (patNames loc p)
     withNames names $
       do p'    <- scPass p
          e'    <- scPass e
          m'    <- scMatch m
          return (MGuard p' e' m')

scMatch m = gmapM scPass m


-- | Resolve names.
scName :: Name -> Scope Name
scName n =
  do qn' <- resolveName (view qualName n)
     return (Lens.set qualName qn' n)


-- Name Gathering --------------------------------------------------------------

-- | Produce the final name map, including any opened modules.
fullNames :: PartialNames -> Scope Names
fullNames (PartialNames ns os) =
  do -- XXX actually import the interfaces here
     return ns

-- | Names and opens defined by a block of declarations.
data PartialNames = PartialNames { pNames :: Names
                                 , pOpens :: Set.Set (Located Open)
                                 } deriving (Show)

instance Monoid PartialNames where
  mempty      = PartialNames { pNames = mempty, pOpens = mempty }
  mappend l r = PartialNames { pNames = mappend (pNames l) (pNames r)
                             , pOpens = mappend (pOpens l) (pOpens r)
                             }

-- | Generate the map from unqualified to qualified, and qualified to qualified
-- names that the top-level of a module defines.
modNames :: Module -> PartialNames
modNames m = PartialNames { pNames = pNames `mappend` qualify namespace pNames
                          , pOpens = pOpens }
  where
  namespace           = unLoc (modName m)
  PartialNames { .. } = foldMap (topDeclNames namespace) (modDecls m)

-- | Generates unqualified to qualified mappings for all names in the TopDecl.
topDeclNames :: ModName -> TopDecl -> PartialNames
topDeclNames ns (TDDecl d)      = declNames ns d
topDeclNames ns (TDData d)      = dataNames ns (unLoc d)
topDeclNames ns (TDPrimType pt) = primTypeNames ns pt
topDeclNames ns (TDPrimTerm pt) = primTermNames ns pt
topDeclNames ns (TDLocal ld)    = localNames ns (unLoc ld)
topDeclNames ns (TDExport e)    = foldMap (topDeclNames ns) (exValue (unLoc e))

declNames :: ModName -> Decl -> PartialNames
declNames ns (DBind b) = bindNames ns (unLoc b)
declNames _  (DSig _)  = mempty
declNames _  (DOpen o) = mempty { pOpens = Set.singleton o }

bindNames :: ModName -> Bind -> PartialNames
bindNames ns Bind { .. } = mempty { pNames = fromDef qn (resolved `at` loc) }
  where
  loc      = getLoc bindName
  qn       = view qualName (unLoc bindName)
  resolved = Qual Expr ns (view qualSymbol qn)

dataNames :: ModName -> DataDecl -> PartialNames
dataNames ns d = mempty

primTypeNames :: ModName -> Located PrimType -> PartialNames
primTypeNames ns lpt =
  mempty { pNames = fromDef qname (lpt {locValue = resolved }) }
  where
  qname    = view qualName (primTypeName (unLoc lpt))
  sym      = view qualSymbol qname
  resolved = Qual (Type 0) ns sym

primTermNames :: ModName -> Located PrimTerm -> PartialNames
primTermNames ns lpt =
  mempty { pNames = fromDef qname (lpt { locValue = resolved }) }
  where
  qname    = view qualName (primTermName (unLoc lpt))
  sym      = view qualSymbol qname
  resolved = Qual Expr ns sym

-- | Gather the names that will be exported by the LocalDecls node.
localNames :: ModName -> LocalDecls -> PartialNames
localNames ns LocalDecls { .. } = foldMap (topDeclNames ns) ldDecls



-- | Gather name mappings for parameters introduced by a pattern.  This name map
-- is important, as it is where Param QualNames get introduced.
patNames :: SrcLoc -> Pat -> PartialNames
patNames loc (PVar n)    = mempty { pNames = fromParam loc n }

-- XXX this may need to generate a module dependency for the constructor in the
-- pattern
patNames loc (PCon c ps) = foldMap (patNames loc) ps

patNames _   (PLoc lp)   = patNames (getLoc lp) (unLoc lp)
patNames _   PLit{}      = mempty
patNames _   PWildcard{} = mempty
