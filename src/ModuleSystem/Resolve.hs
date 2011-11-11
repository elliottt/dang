module ModuleSystem.Resolve (

    -- * Resolved Names
    ResolvedNames
  , mergeResolvedNames
  , Resolved(..), isBound, shadows
  , mergeResolved

    -- * Top-level Names
  , modDecls

    -- * Usage Resolution
  , resolveUses
  ) where

import Interface (IsInterface)
import ModuleSystem.Imports (UseSet)
import QualName (QualName,Name,Namespace,qualName,primName,simpleName)
import Syntax.AST
    (Module(..),modNamespace,PrimType(..),PrimTerm(..),UntypedDecl(..)
    ,TypedDecl(..))
import qualified Data.ClashMap as CM


-- Name Resolution -------------------------------------------------------------

-- | A clash-map from names appearing in the source to names resolved by the
-- import environment.
type ResolvedNames = CM.ClashMap QualName Resolved

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
modDecls :: Module -> ResolvedNames
modDecls m = CM.fromList
           $ concatMap primTypeNames (modPrimTypes m)
          ++ concatMap primTermNames (modPrimTerms m)
          ++ concatMap (typedNames ns) (modTyped m)
          ++ concatMap (untypedNames ns) (modUntyped m)
  where
  ns = modNamespace m


-- | Extract the simple and qualified names that a declaration introduces.
declResolvedNames :: (a -> Name) -> (Name -> QualName)
                  -> (a -> [(QualName,Resolved)])
declResolvedNames simple qualify d = [ (simpleName n, res), (qn, res) ]
  where
  n   = simple d
  qn  = qualify n
  res = Resolved 0 qn

-- | The resolved names from a single typed declaration.
typedNames :: Namespace -> TypedDecl -> [(QualName,Resolved)]
typedNames ns = declResolvedNames typedName (qualName ns)

untypedNames :: Namespace -> UntypedDecl -> [(QualName,Resolved)]
untypedNames ns = declResolvedNames untypedName (qualName ns)

-- | The resolved names form a single primitive type declaration.
primTypeNames :: PrimType -> [(QualName,Resolved)]
primTypeNames  = declResolvedNames primTypeName primName

-- | The resolved names form a single primitive term declaration.
primTermNames :: PrimTerm -> [(QualName,Resolved)]
primTermNames  = declResolvedNames primTermName primName


-- UseSet Interface ------------------------------------------------------------

resolveUses :: IsInterface iface => iface -> UseSet -> ResolvedNames
resolveUses iface = undefined
