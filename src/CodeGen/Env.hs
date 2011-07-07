module CodeGen.Env where

import CodeGen.Types
import Interface
import QualName
import Utils

import Text.LLVM
import qualified Data.Map as Map

data CGEnv = CGEnv
  { cgClosure :: [Typed Value]
  , cgLocals  :: Map.Map Name (Typed Value)
  , cgIface   :: InterfaceSet
  }

emptyCGEnv :: InterfaceSet -> [Typed Value] -> CGEnv
emptyCGEnv iset clos = CGEnv
  { cgIface   = iset
  , cgClosure = clos
  , cgLocals  = Map.empty
  }

type LookupBy a = a -> CGEnv -> Maybe (Typed Value)

lookupArgument :: LookupBy Int
lookupArgument ix env = cgClosure env !!? ix

lookupVar :: LookupBy Name
lookupVar v = Map.lookup v . cgLocals

lookupSymbol :: LookupBy QualName
lookupSymbol qn = fmap declSymbol . lookupFunSymbol qn

addVar :: Name -> Typed Value -> CGEnv -> CGEnv
addVar v tv env = env { cgLocals = Map.insert v tv (cgLocals env) }

instance IsInterface CGEnv where
  lookupFunSymbol qn = lookupFunSymbol qn . cgIface
  funSymbols         = funSymbols         . cgIface
  lookupKind qn      = lookupKind qn      . cgIface
  kinds              = kinds              . cgIface

declSymbol :: FunSymbol -> Typed Value
declSymbol fd = ty -: Symbol (funName fd)
  where
  pho = ptrT heapObjT
  ty  = ptrT (FunTy pho (replicate (funArity fd) pho))
