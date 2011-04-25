module CodeGen.Env where

import CodeGen.Types
import Interface
import ReadWrite
import QualName
import Utils

import Text.LLVM
import qualified Data.Map as Map

data CGEnv = CGEnv
  { cgClosure :: [Typed Value]
  , cgLocals  :: Map.Map Name (Typed Value)
  , cgIface   :: Interface R
  }

emptyCGEnv :: Interface R -> [Typed Value] -> CGEnv
emptyCGEnv iface clos = CGEnv
  { cgIface   = iface
  , cgClosure = clos
  , cgLocals  = Map.empty
  }

type LookupBy a = a -> CGEnv -> Maybe (Typed Value)

lookupArgument :: LookupBy Int
lookupArgument ix env = cgClosure env !!? ix

lookupVar :: LookupBy Name
lookupVar v = Map.lookup v . cgLocals

addVar :: Name -> Typed Value -> CGEnv -> CGEnv
addVar v tv env = env { cgLocals = Map.insert v tv (cgLocals env) }

lookupFunDecl :: QualName -> CGEnv -> Maybe FunDecl
lookupFunDecl qn = findFunDecl qn . cgIface

lookupSymbol :: LookupBy QualName
lookupSymbol qn = fmap declSymbol . lookupFunDecl qn

declSymbol :: FunDecl -> Typed Value
declSymbol fd = ty -: Symbol (funSymbol fd)
  where
  pho = ptrT heapObjT
  ty  = ptrT (FunTy pho (replicate (funArity fd) pho))
