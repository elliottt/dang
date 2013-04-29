module Dang.Core.Interface where

import Dang.Core.AST
    (Module(..),Decl(..),declType,PrimType(..),PrimTerm(..))
import Dang.ModuleSystem.Export (isExported)
import Dang.ModuleSystem.Interface (Interface(..),emptyInterface,Symbol(..),NameMap)
import Dang.QualName (mangle,QualName,primName,qualNamespace,Namespace)
import Dang.TypeChecker.Types (toQual)

import qualified Data.Map as Map


-- | Generate the interface provided by a core module
moduleInterface :: Module -> Interface
moduleInterface m = (emptyInterface qn)
  { ifaceSymbols   = foldl  addDecl         Map.empty (modDecls m)
  , ifacePrimTypes = foldl (addPrimType ns) Map.empty (modPrimTypes m)
  , ifacePrimTerms = foldl (addPrimTerm ns) Map.empty (modPrimTerms m)
  }
  where
  qn = modName m
  ns = qualNamespace qn

-- | Add a declaration to a symbol map.
addDecl :: NameMap Symbol -> Decl -> NameMap Symbol
addDecl syms d
  | isExported d = Map.insert qn sym syms
  | otherwise    =                   syms
  where
  qn  = declName d
  sym = declSymbol qn d

-- | Generate a symbol from a declaration and its fully qualified name.
declSymbol :: QualName -> Decl -> Symbol
declSymbol qn d = Symbol
  { symExternal = mangle qn
  , symInternal = mangle qn
  , symType     = toQual `fmap` declType d
  }

addPrimType :: Namespace -> NameMap PrimType -> PrimType -> NameMap PrimType
addPrimType ns pts pt = Map.insert (primName ns (primTypeName pt)) pt pts

addPrimTerm :: Namespace -> NameMap PrimTerm -> PrimTerm -> NameMap PrimTerm
addPrimTerm ns pts pt = Map.insert (primName ns (primTermName pt)) pt pts
