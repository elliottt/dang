{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Core where

import CodeGen.Env
import CodeGen.PrimOps
import CodeGen.Rts
import CodeGen.Types
import Interface
import LambdaLift
import qualified Syntax.AST as AST

import Data.Int (Int32)
import Data.List (genericLength)
import MonadLib
import Text.LLVM


-- Compilation Monad -----------------------------------------------------------

declFunSpec :: Decl -> FunSpec
declFunSpec d = emptyFunSpec
  { specLinkage = declLinkage d
  }

declLinkage :: Decl -> Maybe Linkage
declLinkage d = do
  guard (not (declExported d))
  return Private

lookupFn :: Monad m => String -> Env -> m (Nat,Fn)
lookupFn n env =
  case envFunDecl n env of
    Nothing -> fail ("lookupFn: " ++ n)
    Just f  -> return (funArity f, simpleFun (funSymbol f))

compModule :: [Decl] -> LLVM Interface
compModule ds = do
  let step (fns,i) d = do
        let fn  = Fun (declName d) (declFunSpec d)
        let sym = FunDecl (funSym fn) (genericLength (declVars d))
        return (fn:fns, addFunDecl (declName d) sym i)
  (fns0,i) <- foldM step ([],emptyInterface) ds
  zipWithM_ (compDecl i) (reverse fns0) ds
  return i

compDecl :: Interface -> Fn -> Decl -> LLVM ()
compDecl i fn d = define fn $ \ rtsEnv ->
   ret =<< compTerm (mkEnv i rtsEnv (declVars d)) (declBody d)

compTerm :: Env -> Term -> BB r (Value Val)
compTerm env t =
  case t of
    Apply c xs  -> compApp env c xs
    Let ds e    -> compLet env ds e
    Symbol s    -> compSymbol env s
    Var n       -> compVar env n
    Argument ix -> argument env ix
    Lit l       -> compLit l
    Prim n a as -> compPrim n a =<< mapM (compTerm env) as

allocValBuffer :: Int32 -> BB r (Value (PtrTo Val))
allocValBuffer len = alloca (toValue len) Nothing

storeValAt :: Value (PtrTo Val) -> Int32 -> Value Val -> BB r ()
storeValAt args ix v = do
  addr <- getelementptr args (ix :: Int32)
  store v addr

-- allocate enough space for vs, load them, call apply and return its result
compApp :: Env -> Call -> [Term] -> BB r (Value Val)
compApp env c xs = do
  vs   <- mapM (compTerm env) xs
  let len = genericLength vs
  args <- allocValBuffer len
  zipWithM_ (storeValAt args) [0..] vs
  clos <- compCall env c
  call rts_apply clos args (toValue len)

compCall :: Env -> Call -> BB r (Value Closure)
compCall env (CFun n) = symbolClosure env n
compCall env (CArg i) = argumentClosure env i

-- | Allocate a new closure that uses the given function symbol.
symbolClosure :: Env -> String -> BB r (Value Closure)
symbolClosure env n = do
  (arity,fn) <- lookupFn n env
  call rts_alloc_closure (toValue arity) (funAddr fn)

-- | Interestingly, if this is the only entry point to argument, no dynamically
-- generated argument indexes can ever be used.
argument :: Env -> Nat -> BB r (Value Val)
argument env = call rts_argument (envClosure env) . toValue

-- | Pull a closure out of the environment, calling rts_barf if the value isn't
-- actually a closure.
argumentClosure :: Env -> Nat -> BB r (Value Closure)
argumentClosure env i = do
  val <- argument env i
  ty  <- call rts_value_type val
  cmp <- icmp Ieq ty valClosure

  exit <- newLabel
  barf <- newLabel
  condBr cmp exit barf
  _        <- defineLabel_ barf (call_ rts_barf >> unreachable)
  (clos,_) <- defineLabel  exit (call rts_get_cval val)

  return clos

-- | Compile non-recursive let-declarations.
compLet :: Env -> [LetDecl] -> Term -> BB r (Value Val)
compLet env0 ds body = flip compTerm body =<< foldM stepDecl env0 ds
  where
  stepDecl env d = do
    (n,v) <- compLetDecl env0 d
    return (addLocal n v env)

-- | Compile a let-declaration.
compLetDecl :: Env -> LetDecl -> BB r (String,Value Val)
compLetDecl env d = name `fmap` compTerm env (letBody d)
  where
  name x = (letName d, x)

compSymbol :: Env -> String -> BB r (Value Val)
compSymbol env s = do
  (n,fn) <- lookupFn s env
  clos   <- call rts_alloc_closure (toValue n) (funAddr fn)
  cval   <- call rts_alloc_value valClosure
  call_ rts_set_cval cval clos
  return cval

compVar :: Env -> String -> BB r (Value Val)
compVar env n =
  case lookupLocal n env of
    Nothing -> fail ("Unknown local variable: " ++ n)
    Just v  -> return v

compLit :: AST.Literal -> BB r (Value Val)
compLit (AST.LInt i) = do
  ival <- call rts_alloc_value valInt
  call_ rts_set_ival ival (toValue i)
  return ival

compPrim :: String -> Int -> [Value Val] -> BB r (Value Val)
compPrim n arity ts =
  case arity of
    1 -> do
      let [a] = ts
      case n of
        "prim_abs_i"    -> primAbs a
        "prim_signum_i" -> error "prim_signum_i"
        _            -> fail ("unknown primitive: " ++ n)

    2 -> do
      let [a,b] = ts
      case n of
        "prim_add_i" -> intPrimBinop add a b
        "prim_sub_i" -> intPrimBinop sub a b
        "prim_mul_i" -> intPrimBinop mul a b
        _            -> fail ("unknown primitive: " ++ n)

    _ -> fail ("unknown primitive: " ++ n)
