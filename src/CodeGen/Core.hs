{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Core where

import CodeGen.PrimOps
import CodeGen.Rts
import CodeGen.Types
import Interface
import LambdaLift
import qualified AST

import Data.Int (Int32)
import Data.List (genericLength)
import MonadLib
import Text.LLVM


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

compTerm :: Interface -> [String] -> Value RtsEnv -> Term -> BB r (Value Val)
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
compApp :: Interface -> [String] -> Value RtsEnv -> Call -> [Term]
        -> BB r (Value Val)
compApp i env rtsEnv c xs = do
  vs   <- mapM (compTerm i env rtsEnv) xs
  let len = genericLength vs
  args <- allocValBuffer len
  zipWithM_ (storeValAt args) [0..] vs
  clos <- compCall i rtsEnv c
  call rts_apply clos args (toValue len)

compCall :: Interface -> Value RtsEnv -> Call -> BB r (Value Closure)
compCall i _      (CFun n) = symbolClosure i n
compCall _ rtsEnv (CArg i) = argumentClosure rtsEnv i

-- | Allocate a new closure that uses the given function symbol.
symbolClosure :: Interface -> String -> BB r (Value Closure)
symbolClosure i n = do
  (arity,fn) <- lookupFn n i
  call rts_alloc_closure (toValue arity) (funAddr fn)

-- | Pull a closure out of the environment, calling rts_barf if the value isn't
-- actually a closure.
argumentClosure :: Value RtsEnv -> Nat -> BB r (Value Closure)
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
compLet :: Interface -> [String] -> Value RtsEnv -> [Decl] -> Term
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

compArgument :: Value RtsEnv -> Int32 -> BB r (Value Val)
compArgument rtsEnv i = call rts_argument rtsEnv (toValue i)

compLit :: AST.Literal -> BB r (Value Val)
compLit (AST.LInt i) = do
  ival <- call rts_alloc_value valInt
  call_ rts_set_ival ival (toValue i)
  return ival

compPrim :: Interface -> [String] -> Value RtsEnv -> String -> Int
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
