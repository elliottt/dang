module Core.Interface where

import Core.AST (Module(..),Export(..),Decl(..),declType,PrimType(..))
import ModuleSystem.Interface (Interface(..),emptyInterface,Symbol(..),NameMap)
import QualName (mangle,QualName,primName,qualNamespace,Namespace)

import qualified Data.Map as Map


-- | Generate the interface provided by a core module
moduleInterface :: Module -> Interface
moduleInterface m = (emptyInterface qn)
  { ifaceSymbols   = foldl  addDecl         Map.empty (modDecls m)
  , ifacePrimTypes = foldl (addPrimType ns) Map.empty (modPrimTypes m)
  }
  where
  qn = modName m
  ns = qualNamespace qn

-- | Add a declaration to a symbol map.
addDecl :: NameMap Symbol -> Decl -> NameMap Symbol
addDecl syms d
  | declExport d == Public = Map.insert qn sym syms
  | otherwise              =                   syms
  where
  qn  = declName d
  sym = declSymbol qn d

-- | Generate a symbol from a declaration and its fully qualified name.
declSymbol :: QualName -> Decl -> Symbol
declSymbol qn d = Symbol
  { symExternal = mangle qn
  , symInternal = mangle qn
  , symType     = declType d
  }

addPrimType :: Namespace -> NameMap PrimType -> PrimType -> NameMap PrimType
addPrimType ns pts pt = Map.insert (primName ns (primTypeName pt)) pt pts
