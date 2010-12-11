module CodeGen.Env where

import CodeGen.Types
import Interface

import Control.Monad (msum)
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
envFunDecl :: String -> Env -> Maybe FunDecl
envFunDecl n = findFunDecl n . envInterface

-- | Lookup a value in the environment of values that have been seen by the code
-- generator.
envValue :: String -> Env -> Maybe (Value Val)
envValue n = Map.lookup n . envLocal

-- | Determine whether or not a variable is from the closure.
isFromClosure :: String -> Env -> Bool
isFromClosure n = Set.member n . envArgs

-- | Add a local variable to the environment.
addLocal :: String -> Value Val -> Env -> Env
addLocal n v env = env { envLocal = Map.insert n v (envLocal env) }

lookupLocal :: String -> Env -> Maybe (Value Val)
lookupLocal n = Map.lookup n . envLocal
