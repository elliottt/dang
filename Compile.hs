{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compile where

import Interface
import LambdaLift
import Pretty
import qualified AST

import Data.Int (Int32,Int64)
import Data.List (elemIndex,genericLength)
import MonadLib
import Text.LLVM


-- RTS Types -------------------------------------------------------------------

type Nat = Int32


newtype Closure = Closure (PtrTo Int32)

instance IsType Closure where
  getType (Closure ptr)= getType ptr

instance HasValues Closure


newtype Val = Val (PtrTo Int32)

instance IsType Val where
  getType (Val ptr) = getType ptr

instance HasValues Val


type ValType = Int32

valInt, valClosure :: Value ValType
valInt     = toValue 0x0
valClosure = toValue 0x1


-- RTS Primitives --------------------------------------------------------------

rts_argument :: Fun (Closure -> Nat -> Res Val)
rts_argument  = Fun
  { funSym     = "argument"
  , funLinkage = Nothing
  }

rts_apply :: Fun (Closure -> PtrTo Val -> Nat -> Res Val)
rts_apply  = Fun
  { funSym     = "apply"
  , funLinkage = Nothing
  }

rts_alloc_closure :: Fun (Nat -> Fn -> Res Closure)
rts_alloc_closure  = Fun
  { funSym     = "alloc_closure"
  , funLinkage = Nothing
  }

rts_alloc_value :: Fun (ValType -> Res Val)
rts_alloc_value  = Fun
  { funSym     = "alloc_value"
  , funLinkage = Nothing
  }

rts_set_ival :: Fun (Val -> Int64 -> Res ())
rts_set_ival  = Fun
  { funSym     = "set_ival"
  , funLinkage = Nothing
  }

rts_set_cval :: Fun (Val -> Closure -> Res ())
rts_set_cval  = Fun
  { funSym     = "set_cval"
  , funLinkage = Nothing
  }

rtsImports :: LLVM ()
rtsImports  = do
  declare rts_argument
  declare rts_apply
  declare rts_alloc_closure
  declare rts_alloc_value
  declare rts_set_ival


-- Compilation Monad -----------------------------------------------------------

type Fn = Fun (Closure -> Res Val)

declLinkage :: Decl -> Maybe Linkage
declLinkage d
  | declExported d = Nothing
  | otherwise      = Just Private

lookupFn :: Monad m => String -> Interface -> m (Nat,Fn)
lookupFn n i =
  case findFunDecl n i of
    Nothing -> fail ("lookupFn: " ++ n)
    Just f  -> return (funArity f, fn)
      where
      fn = Fun
        { funSym     = funSymbol f
        , funLinkage = Nothing
        }

compModule :: [Decl] -> LLVM Interface
compModule ds = do
  let step (fns,i) d = do
        fn <- newFun (declLinkage d)
        let sym = FunDecl (funSym fn) (genericLength (declVars d))
        return (fn:fns, addFunDecl (declName d) sym i)
  (fns0,i) <- foldM step ([],emptyInterface) ds
  zipWithM_ (compDecl i) (reverse fns0) ds
  return i

compDecl :: Interface -> Fn -> Decl -> LLVM ()
compDecl i fn d = define fn $ \ env ->
  ret =<< compTerm i (declVars d) env (declBody d)

compTerm :: Interface -> [String] -> Value Closure -> Term -> BB r (Value Val)
compTerm i env rtsEnv t =
  case t of
    Apply c xs -> compApp i env rtsEnv c xs
    Let ds e   -> compLet i env rtsEnv ds e
    Symbol s   -> compSymbol i s
    Argument i -> compArgument rtsEnv i
    Lit l      -> compLit l

compApp :: Interface -> [String] -> Value Closure -> Call -> [Term]
        -> BB r (Value Val)
compApp i env rtsEnv c xs = do
  vs   <- mapM (compTerm i env rtsEnv) xs
  cvar <- compCall i rtsEnv c
  -- allocate enough space for vs, load them, call apply and return its result
  --args <- alloca (toValue (genericLength vs)) Nothing
  undefined

compCall = undefined

compLet :: Interface -> [String] -> Value Closure -> [Decl] -> Term
        -> BB r (Value Val)
compLet i env rtsEnv ds e = undefined

compSymbol :: Interface -> String -> BB r (Value Val)
compSymbol i s = do
  (n,fn) <- lookupFn s i
  clos   <- call rts_alloc_closure (toValue n) (toValue fn)
  cval   <- call rts_alloc_value valClosure
  call_ rts_set_cval cval clos
  return cval

compArgument :: Value Closure -> Int32 -> BB r (Value Val)
compArgument rtsEnv i = call rts_argument rtsEnv (toValue i)

compLit :: AST.Literal -> BB r (Value Val)
compLit (AST.LInt i) = do
  ival <- call rts_alloc_value valInt
  call_ rts_set_ival ival (toValue i)
  return ival
