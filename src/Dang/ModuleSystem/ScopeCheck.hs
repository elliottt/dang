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

import           Control.Lens as Lens ( view, set, traverseOf )
import           Data.Generics ( Data, gmapM, extM, ext1M )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           MonadLib
                     ( BaseM(..), runM, Id, ReaderT, ask, local, StateT, get
                     , set )


-- | Fully-qualify all names in the module, according to their imports.
scopeCheckModule :: Module -> Dang Module
scopeCheckModule m = failErrs (runScope (scModule m))

scPanic :: Pretty msg => msg -> a
scPanic  = panic "Dang.ModuleSystem.ScopeCheck"


-- Scope Checking Monad --------------------------------------------------------

data RW = RW { rwNext   :: Map.Map String Int
             , rwIfaces :: Map.Map ModName Iface
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
                               RW { rwNext  = mempty, rwIfaces  = Map.empty }
     return a


-- | Locally override the current namespace used when qualifying names.
withModName :: ModName -> Scope a -> Scope a
withModName ns m = Scope $
  do ro <- ask
     local ro { roModName = ns } (unScope m)

getModName :: Scope ModName
getModName  = Scope (roModName `fmap` ask)

-- | Reset the naming environment.
withNames :: Names -> Scope a -> Scope a
withNames names m = Scope $
  do ro <- ask
     local ro { roNames = names } (unScope m)

-- | Introduce a group of names in a scope, shadowing existing names that would
-- have overlapped.
--
-- XXX this needs to emit warnings when shadowing occurs.
extNames :: Names -> Scope a -> Scope a
extNames names m =
  do names' <- freshen names
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

-- | Generate a naming environment from a declaration.  From the module:
--
-- > module A where
-- > f x = 10
--
-- The naming environment can be generated from this call:
--
-- > fromDef [| f |] [| A.f |]
--
fromDef :: ModName -> Located Name -> Names
fromDef ns ln =
  Names { nDefs = Map.singleton sourceName [FromBind (realName `at` ln)] }
  where
  sourceName = view qualName (unLoc ln)
  realName   = Qual (view qualLevel sourceName) ns (view qualSymbol sourceName)

-- | Generate a naming environment from a parameter.  From the declaration
--
-- > f x = ...
--
-- the naming environment for `x` can be generated from this call:
--
-- > fromParam [| x |]
--
-- `x` is assumed to be unqualified.
fromParam :: SrcLoc -> Name -> Names
fromParam loc n = Names { nDefs = Map.singleton qn [FromParam (qn `at` loc)] }
  where
  qn = unqual (view qualName n)

-- | Generate a naming environment from an import declaration.  The name will be
-- qualified according to the `as` clause of the declaration, with no
-- qualification if the `as` clause is missing.
fromImport :: Located Open -> Name -> Names
fromImport lo n = Names { nDefs = Map.singleton key [FromIface lo qn] }
  where
  qn  = view qualName n
  ns  = maybe [] unLoc (openAs (unLoc lo))
  key = Lens.set qualModule ns qn


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
   `extM` scExpr
   `extM` scName


-- | Rename a module.
scModule :: Module -> Scope Module
scModule m = withModName (unLoc (modName m)) $
  do names <- fullNames (modNames m)
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
     extNames lNames $
       do ldLocals <- scPass ldLocals
          ldDecls <- scPass ldDecls
          return LocalDecls { .. }


-- | Introduce the names bound by the pattern, when checking the body of the
-- Match.
scMatch :: Match -> Scope Match

scMatch (MPat p m) =
  do loc   <- askLoc
     names <- fullNames (patNames loc p)
     extNames names $
       do p'    <- scPass p
          m'    <- scMatch m
          return (MPat p' m')

scMatch (MGuard p e m) =
  do loc   <- askLoc
     names <- fullNames (patNames loc p)
     extNames names $
       do p'    <- scPass p
          e'    <- scPass e
          m'    <- scMatch m
          return (MGuard p' e' m')

scMatch m = gmapM scPass m


-- | The only interesting case for name introduction in expressions is Let, as
-- it can define new names, as well as import other modules.
scExpr :: Expr -> Scope Expr

scExpr (Let ds e) =
  do ns    <- getModName
     names <- fullNames (foldMap (declNames ns) ds)
     extNames names $
       do ds' <- mapM scPass ds
          e'  <- scPass e
          return (Let ds' e')

scExpr e = gmapM scPass e


-- | Resolve names.
scName :: Name -> Scope Name
scName n =
  do qn' <- resolveName (view qualName n)
     return (Lens.set qualName qn' n)


-- Interface Files -------------------------------------------------------------

-- | Load, and cache an interface file.
loadIface :: ModName -> Scope Iface
loadIface name = Scope $
  fail "loadIface"
  -- do rw <- get
  --    case Map.lookup name (rwIfaces rw) of
  --      Just iface -> return iface
  --      Nothing    -> do iface <- inBase (readIface name)
  --                       MonadLib.set rw { rwIfaces = Map.insert name iface (rwIfaces rw) }
  --                       return iface

openNames :: Located Open -> Scope Names
openNames lo =
  do iface <- loadIface (unLoc openMod)
     let Names ns = ifaceNames lo iface
     return (Names (Map.filterWithKey adjust ns))
  where
  Open { .. } = unLoc lo

  syms = concatMap (openedSyms . unLoc) openSymbols

  openedSyms (OpenTerm s)    = [s]
  openedSyms (OpenType t cs) = t:cs

  -- This predicate handles four cases:
  --
  --  1. open Foo hiding ()   - import everything
  --  2. open Foo        ()   - import nothing
  --  3. open Foo hiding (..) - don't import (..)
  --  4. open Foo        (..) - only import (..)
  --
  adjust | null syms  = \ _  _ -> openHiding
         | openHiding = \ qn _ -> view qualSymbol qn `notElem` syms
         | otherwise  = \ qn _ -> view qualSymbol qn `elem`    syms

  rename | Just ln <- openAs = qualify (unLoc ln)
         | otherwise         = id


-- | All names defined by an interface.
ifaceNames :: Located Open -> Iface -> Names
ifaceNames lo Iface { .. } = foldMap (ifaceDefNames lo) ifaceDefs

ifaceDefNames :: Located Open -> IfaceDef -> Names
ifaceDefNames lo (IfaceDefBind b) = ifaceBindNames lo b

ifaceBindNames :: Located Open -> IfaceBind -> Names
ifaceBindNames lo IfaceBind { .. } = fromImport lo ibName


-- Name Gathering --------------------------------------------------------------

-- | Produce the final name map, including any opened modules.
fullNames :: PartialNames -> Scope Names
fullNames (PartialNames ns os) =
  do namess <- mapM openNames (Set.toList os)
     return (mconcat (ns:namess))

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
topDeclNames ns (TDData d)      = mempty { pNames = dataNames     ns d  }
topDeclNames ns (TDPrimType pt) = mempty { pNames = primTypeNames ns pt }
topDeclNames ns (TDPrimTerm pt) = mempty { pNames = primTermNames ns pt }
topDeclNames ns (TDLocal ld)    = localNames ns (unLoc ld)
topDeclNames ns (TDExport e)    = foldMap (topDeclNames ns) (exValue (unLoc e))

declNames :: ModName -> Decl -> PartialNames
declNames ns (DBind b) = mempty { pNames = bindNames ns (unLoc b) }
declNames _  (DSig _)  = mempty
declNames _  (DOpen o) = mempty { pOpens = Set.singleton o }

bindNames :: ModName -> Bind -> Names
bindNames ns Bind { .. } = fromDef ns bindName


dataNames :: ModName -> Located DataDecl -> Names
dataNames ns ld = tyName `mappend` groupNames
  where
  tyName     = fromDef ns (dataName `fmap` ld)
  groupNames = foldMap (constrGroupNames ns) (dataGroups (unLoc ld))

constrGroupNames :: ModName -> Located ConstrGroup -> Names
constrGroupNames ns lcg = foldMap (constrNames ns) (groupConstrs (unLoc lcg))

constrNames :: ModName -> Located Constr -> Names
constrNames ns lc = fromDef ns (constrName `fmap` lc)


primTypeNames :: ModName -> Located PrimType -> Names
primTypeNames ns lpt = fromDef ns (primTypeName `fmap` lpt)

primTermNames :: ModName -> Located PrimTerm -> Names
primTermNames ns lpt = fromDef ns (primTermName `fmap` lpt)

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
