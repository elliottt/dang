{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compile where

import Interface
import LambdaLift
import qualified AST

import Data.Bits (complement,bit)
import Data.Int (Int32,Int64)
import Data.List (genericLength)
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

rts_alloc_closure :: Fun (Nat -> PtrTo Fn -> Res Closure)
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

rts_get_ival :: Fun (Val -> Res Int64)
rts_get_ival  = Fun
  { funSym     = "get_ival"
  , funLinkage = Nothing
  }

rts_get_cval :: Fun (Val -> Res Closure)
rts_get_cval  = Fun
  { funSym     = "get_cval"
  , funLinkage = Nothing
  }

rts_get_type :: Fun (Val -> Res Int32)
rts_get_type  = Fun
  { funSym     = "get_type"
  , funLinkage = Nothing
  }

rts_barf :: Fun (Res ())
rts_barf  = Fun
  { funSym     = "barf"
  , funLinkage = Nothing
  }

rtsImports :: LLVM ()
rtsImports  = do
  declare rts_argument
  declare rts_apply
  declare rts_alloc_closure
  declare rts_alloc_value
  declare rts_set_ival
  declare rts_get_ival
  declare rts_set_cval
  declare rts_get_cval
  declare rts_get_type
  declare rts_barf


-- Primitives ------------------------------------------------------------------

type PrimUnary = Fun (Val -> Res Val)

prim_abs_i :: PrimUnary
prim_abs_i  = Fun "prim_abs_i" Nothing

prim_signum_i :: PrimUnary
prim_signum_i  = Fun "prim_signum_i" Nothing

rtsPrims :: LLVM ()
rtsPrims  = do
  declare prim_abs_i
  declare prim_signum_i


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
        fn <- newNamedFun (declName d) (declLinkage d)
        let sym = FunDecl (funSym fn) (genericLength (declVars d))
        return (fn:fns, addFunDecl (declName d) sym i)
  (fns0,i) <- foldM step ([],emptyInterface) ds
  zipWithM_ (compDecl i) (reverse fns0) ds
  return i

compDecl :: Interface -> Fn -> Decl -> LLVM ()
compDecl i fn d = define fn $ \ rtsEnv ->
  ret =<< compTerm i (declVars d) rtsEnv (declBody d)

compTerm :: Interface -> [String] -> Value Closure -> Term -> BB r (Value Val)
compTerm i env rtsEnv t =
  case t of
    Apply c xs  -> compApp i env rtsEnv c xs
    Let ds e    -> compLet i env rtsEnv ds e
    Symbol s    -> compSymbol i s
    Argument ix -> compArgument rtsEnv ix
    Lit l       -> compLit l
    Prim n a as -> compPrim i env rtsEnv n a =<< mapM (compTerm i env rtsEnv) as

allocValBuffer :: Int32 -> BB r (Value (PtrTo Val))
allocValBuffer len = alloca (toValue len) Nothing

storeValAt :: Value (PtrTo Val) -> Int32 -> Value Val -> BB r ()
storeValAt args ix v = do
  addr <- getelementptr args (ix :: Int32)
  store v addr

-- allocate enough space for vs, load them, call apply and return its result
compApp :: Interface -> [String] -> Value Closure -> Call -> [Term]
        -> BB r (Value Val)
compApp i env rtsEnv c xs = do
  vs   <- mapM (compTerm i env rtsEnv) xs
  let len = genericLength vs
  args <- allocValBuffer len
  zipWithM_ (storeValAt args) [0..] vs
  clos <- compCall i rtsEnv c
  call rts_apply clos args (toValue len)

compCall :: Interface -> Value Closure -> Call -> BB r (Value Closure)
compCall i _      (CFun n) = symbolClosure i n
compCall _ rtsEnv (CArg i) = argumentClosure rtsEnv i

-- | Allocate a new closure that uses the given function symbol.
symbolClosure :: Interface -> String -> BB r (Value Closure)
symbolClosure i n = do
  (arity,fn) <- lookupFn n i
  call rts_alloc_closure (toValue arity) (funAddr fn)

-- | Pull a closure out of the environment, calling rts_barf if the value isn't
-- actually a closure.
argumentClosure :: Value Closure -> Nat -> BB r (Value Closure)
argumentClosure rtsEnv i = do
  val <- call rts_argument rtsEnv (toValue i)
  ty  <- call rts_get_type val
  cmp <- icmp Ieq ty valClosure

  exit <- newLabel
  barf <- newLabel
  condBr cmp exit barf
  _        <- defineLabel_ barf (call_ rts_barf >> unreachable)
  (clos,_) <- defineLabel  exit (call rts_get_cval val)

  return clos

-- This requires a mechanism that isn't really present yet... A local
-- environment.
compLet :: Interface -> [String] -> Value Closure -> [Decl] -> Term
        -> BB r (Value Val)
compLet = error "compLet"
--compLet i env rtsEnv ds e = error "compLet"

compSymbol :: Interface -> String -> BB r (Value Val)
compSymbol i s = do
  (n,fn) <- lookupFn s i
  clos   <- call rts_alloc_closure (toValue n) (funAddr fn)
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

compPrim :: Interface -> [String] -> Value Closure -> String -> Int
         -> [Value Val] -> BB r (Value Val)
compPrim i env rtsEnv n arity ts =
  case arity of
    1 -> do
      let [a] = ts
      case n of
        "prim_abs_i"    -> primAbs a
        "prim_signum_i" -> call prim_signum_i a
        _            -> fail ("unknown primitive: " ++ n)

    2 -> do
      let [a,b] = ts
      case n of
        "prim_add_i" -> intPrimBinop add a b
        "prim_sub_i" -> intPrimBinop sub a b
        "prim_mul_i" -> intPrimBinop mul a b
        _            -> fail ("unknown primitive: " ++ n)

    _ -> fail ("unknown primitive: " ++ n)

-- | Lift an Int64 primitive over Vals.
intPrimBinop :: (Value Int64 -> Value Int64 -> BB r (Value Int64))
             -> Value Val -> Value Val -> BB r (Value Val)
intPrimBinop k a b = do
  aI  <- call rts_get_ival a
  bI  <- call rts_get_ival b
  res <- k aI bI
  val <- call rts_alloc_value valInt
  call_ rts_set_ival val res
  return val

primAbs :: Value Val -> BB r (Value Val)
primAbs a = do
  aI <- call rts_get_ival a

  pos <- newLabel
  neg <- newLabel
  out <- newLabel
  b   <- icmp Ilt aI (toValue 0)
  condBr b neg pos

  posOut <- defineLabel pos $ do
    br out
    return a

  negOut <- defineLabel neg $ do
    val <- call rts_alloc_value valInt
    call_ rts_set_ival val =<< sub (toValue 0) aI
    br out
    return val

  defineLabel_ out (phi negOut posOut)
