module CodeGen.Env where

import CodeGen.Types
import Interface
import QualName

import Text.LLVM (Value)
import qualified Data.Map as Map
import qualified Data.Set as Set


data Env = Env
  { envInterface :: Interface
  , envClosure   :: Value RtsEnv
  , envArgs      :: Set.Set String
  , envLocal     :: Map.Map String (Value Val)
  }

mkEnv :: Interface -> Value RtsEnv -> [String] -> Env
mkEnv i rtsEnv args = Env
  { envInterface = i
  , envClosure   = rtsEnv
  , envArgs      = Set.fromList args
  , envLocal     = Map.empty
  }

-- | Lookup a function definition out of the local environment.
envFunDecl :: QualName -> Env -> Maybe FunDecl
envFunDecl n = findFunDecl n . envInterface

-- | Lookup a value in the environment of values that have been seen by the code
-- generator.
envValue :: String -> Env -> Maybe (Value Val)
envValue n = Map.lookup n . envLocal

-- | Add a local variable to the environment.
addLocal :: String -> Value Val -> Env -> Env
addLocal n v env = env { envLocal = Map.insert n v (envLocal env) }

lookupLocal :: String -> Env -> Maybe (Value Val)
lookupLocal n = Map.lookup n . envLocal
