module ModuleSystem.Resolve (

    -- * Resolved Names
    ResolvedNames
  , emptyResolvedNames
  , mergeResolvedNames
  , Resolved(..), isBound, shadows
  , mergeResolved

    -- * Top-level Names
  , modResolvedNames

    -- * Usage Resolution
  , resolveUses
  ) where

import ModuleSystem.Interface (InterfaceSet,lookupInterface,ifaceNames)
import ModuleSystem.Imports (UseSet,Use(..))
import ModuleSystem.Types (UsedName(..),simpleUsedName,mapUsedName,usedQualName)
import QualName
    (QualName,Name,Namespace,qualName,primName,simpleName,changeNamespace
    ,qualNamespace)
import Syntax.AST
    (Module(..),modNamespace,PrimType(..),PrimTerm(..),UntypedDecl(..)
    ,TypedDecl(..),DataDecl(..),ConstrGroup(..),Constr(..),Open(..)
    ,OpenSymbol(..))
import TypeChecker.Types (forallData)
import qualified Data.ClashMap as CM

import Data.List (foldl')
import qualified Data.Set as Set


-- Name Resolution -------------------------------------------------------------

-- | A clash-map from names appearing in the source to names resolved by the
-- import environment.
type ResolvedNames = CM.ClashMap UsedName Resolved

emptyResolvedNames :: ResolvedNames
emptyResolvedNames  = CM.empty

-- | Lexical scope numbering.
type Level = Int

-- | Names resolved by an interface, or function environment.
data Resolved
  = Resolved Level QualName -- ^ A resolved, top-level name
  | Bound Name              -- ^ A resolved bound name
    deriving (Eq,Show)

-- | True when the resolved name is a bound variable.
isBound :: Resolved -> Bool
isBound Bound{} = True
isBound _       = False

-- | True when the left argument shadows the right argument by belonging to a
-- deeper scope.
shadows :: Resolved -> Resolved -> Bool
shadows (Resolved i _) (Resolved j _) = i > j
shadows _              _              = False

-- | Merge resolved names.
mergeResolved :: CM.Strategy Resolved
mergeResolved a b
  | isBound a     = CM.ok a
  | a == b        = CM.ok a
  | a `shadows` b = CM.ok a
  | otherwise     = CM.clash a b

-- | Merge two name resolution maps, allowing for shadowing.  The left
-- substitution will take precedence over the right one.
mergeResolvedNames :: ResolvedNames -> ResolvedNames -> ResolvedNames
mergeResolvedNames  = CM.unionWith mergeResolved


-- Defined Names ---------------------------------------------------------------

-- | Generate the set of resolved names that a module defines at the top-level.
modResolvedNames :: Module -> ResolvedNames
modResolvedNames m = CM.fromList
                   $ concatMap (dataDeclNames ns) (modDatas m)
                  ++ concatMap (primTypeNames ns) (modPrimTypes m)
                  ++ concatMap (primTermNames ns) (modPrimTerms m)
                  ++ concatMap (typedNames ns)    (modTyped m)
                  ++ concatMap (untypedNames ns)  (modUntyped m)
  where
  ns = modNamespace m

-- | Extract the simple and qualified names that a declaration introduces.
declResolvedNames :: (a -> Name) -> (QualName -> UsedName) -> (Name -> QualName)
                  -> (a -> [(UsedName,Resolved)])
declResolvedNames simple k qualify d =
  [ (k (simpleName n), res), (u, res) ]
  where
  n   = simple d
  qn  = qualify n
  u   = k qn
  res = Resolved 0 qn

-- | The resolved names from a data declaration.
dataDeclNames :: Namespace -> DataDecl -> [(UsedName,Resolved)]
dataDeclNames ns d = declResolvedNames dataName UsedType (qualName ns) d
                  ++ concatMap (constrGroupNames ns . forallData) (dataGroups d)

-- | The resolved names from a group of constructors.
constrGroupNames :: Namespace -> ConstrGroup -> [(UsedName,Resolved)]
constrGroupNames ns = concatMap (constrNames ns) . groupConstrs

-- | The resolved names from a data constructor.
constrNames :: Namespace -> Constr -> [(UsedName,Resolved)]
constrNames  = declResolvedNames constrName UsedTerm . qualName

-- | The resolved names from a single typed declaration.
typedNames :: Namespace -> TypedDecl -> [(UsedName,Resolved)]
typedNames  = declResolvedNames typedName UsedTerm . qualName

untypedNames :: Namespace -> UntypedDecl -> [(UsedName,Resolved)]
untypedNames  = declResolvedNames untypedName UsedTerm . qualName

-- | The resolved names form a single primitive type declaration.
primTypeNames :: Namespace -> PrimType -> [(UsedName,Resolved)]
primTypeNames  = declResolvedNames primTypeName UsedType . primName

-- | The resolved names form a single primitive term declaration.
primTermNames :: Namespace -> PrimTerm -> [(UsedName,Resolved)]
primTermNames  = declResolvedNames primTermName UsedTerm . primName


-- UseSet Interface ------------------------------------------------------------

-- | Resolve a usage set using a set of interfaces that satisfy it.
resolveUses :: InterfaceSet -> UseSet -> ResolvedNames
resolveUses iset = foldl' step CM.empty . Set.toList
  where
  step res (QualTerm qn) = resolveQualName UsedTerm qn `mergeResolvedNames` res
  step res (QualType qn) = resolveQualName UsedType qn `mergeResolvedNames` res
  step res (Explicit o)  = resolveOpen iset o `mergeResolvedNames` res

-- | Resolve a qualified name into a used name.
resolveQualName :: (QualName -> UsedName) -> QualName -> ResolvedNames
resolveQualName k qn = CM.singleton (k qn) (Resolved 0 qn)

-- | Resolve an open declaration into a set of resolved names.
resolveOpen :: InterfaceSet -> Open -> ResolvedNames
resolveOpen iset o = rename resolved
  where
  ns                      = qualNamespace (openMod o)
  syms                    = resolveModule iset (openMod o)
  resolved | openHiding o = resolveHiding (openSymbols o) syms
           | otherwise    = resolveOnly   ns (openSymbols o) syms
  rename = case openAs o of
    Nothing -> id
    Just m' -> CM.mapKeys (mapUsedName (changeNamespace (qualNamespace m')))

-- | Resolve all symbols from a module as though they were opened with no
-- qualifications.
resolveModule :: InterfaceSet -> QualName -> ResolvedNames
resolveModule iset m = case lookupInterface m iset of
  Just iface ->
    CM.fromListWith mergeResolved (map step (ifaceNames iface))
  Nothing    -> error "ModuleSystem.Resolve.resolveModule: inconceivable"
  where
  step u = (simpleUsedName u, Resolved 0 (usedQualName u))

-- | Resolve an open declaration that hides some names, and optionally renames
-- the module.
resolveHiding :: [OpenSymbol] -> ResolvedNames -> ResolvedNames
resolveHiding os syms = foldl step syms os
  where
  step m o = case o of
    OpenTerm n    -> CM.delete (UsedTerm (simpleName n)) m
    OpenType n cs ->
      let keys = UsedType (simpleName n) : map (UsedTerm . simpleName) cs
       in CM.filterWithKey (\k _ -> k `elem` keys) m

-- | Resolve an open declaration that selects some names, and optionally renames
-- the module.
resolveOnly :: Namespace -> [OpenSymbol] -> ResolvedNames -> ResolvedNames
resolveOnly ns os syms = CM.intersection syms (CM.fromList (concatMap step os))
  where
  step sym = case sym of
    OpenTerm n    -> [openTerm ns n]
    OpenType t cs -> openType ns t : map (openTerm ns) cs

-- | Construct an element of a clash map that resolves a used name to its
-- resolved name.
openTerm :: Namespace -> Name -> (UsedName,Resolved)
openTerm ns n =
  (UsedTerm (simpleName n), Resolved 0 (qualName ns n))

-- | Construct an element of a clash map that resolves a used name to its
-- resolved name.
openType :: Namespace -> Name -> (UsedName,Resolved)
openType ns n =
  (UsedType (simpleName n), Resolved 0 (qualName ns n))
