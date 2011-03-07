{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Core where

import CodeGen.Env
import CodeGen.PrimOps
import CodeGen.Rts
import CodeGen.Types
import Interface
import LambdaLift
import Pretty
import QualName
import ReadWrite
import qualified Syntax.AST as AST
import qualified QualNameMap as QN

import Data.Int (Int32)
import Data.List (genericLength,intercalate)
import MonadLib
import Text.LLVM
import Text.LLVM.AST (Linkage(..),GC(..),ICmpOp(..))

enableGC :: Bool
enableGC  = True

-- Compilation Monad -----------------------------------------------------------

sanitize :: String -> String
sanitize  = concatMap escape
  where
  escape '_' = "__"
  escape c   = [c]

mangleName :: QualName -> Nat -> Name
mangleName (PrimName n)    _ = n
mangleName (QualName ps n) a =
  "_cv" ++ intercalate "_" (map sanitize ps ++ [sanitize n]) ++ show a

declFunAttrs :: Decl -> FunAttrs
declFunAttrs d = emptyFunAttrs
  { funLinkage = declLinkage d
  , funGC      = guard enableGC >> Just (GC "shadow-stack")
  }

declLinkage :: Decl -> Maybe Linkage
declLinkage d = do
  guard (declExport d == AST.Private)
  return Private

lookupCode :: Monad m => QualName -> Env -> m (Nat,Code)
lookupCode n env =
  case envFunDecl n env of
    Nothing -> fail ("lookupCode: " ++ pretty n)
    Just f  -> return (funArity f, simpleFun (funSymbol f))

externCode :: FunDecl -> Code
externCode fd = simpleFun (funSymbol fd)

externDecls :: Interface R -> LLVM ()
externDecls i = mapM_ step (QN.toList (intFunDecls i))
  where
  step (_,fn) = declare (externCode fn)

compModule :: Interface R -> [Decl] -> LLVM (Interface RW)
compModule i0 ds = do
  externDecls i0
  let step (fns,i) d = do -- why is this monadic?
        let arity = genericLength (declVars d)
        let name  = mangleName (declName d) arity
        let fn    = funWithAttrs name (declFunAttrs d)
        let sym   = FunDecl name arity
        let var   = declName d
        return (fn:fns, addFunDecl var sym i)
  (fns0,i) <- foldM step ([],emptyInterface) ds
  let i' = mergeInterfaces i i0
  zipWithM_ (compDecl i') (reverse fns0) ds
  return i

compDecl :: Interface R -> Code -> Decl -> LLVM ()
compDecl i fn d = define fn $ \ rtsEnv ->
   ret =<< compTerm (mkEnv i rtsEnv (declVars d)) (declBody d)

compTerm :: Env -> Term -> BB Val (Value Val)
compTerm env t =
  case t of
    Apply c xs  -> compApp env c xs
    Let ds e    -> compLet env ds e
    Symbol qn   -> compSymbol env qn
    Var n       -> compVar env n
    Argument ix -> argument env ix
    Lit l       -> compLit l
    Prim n a as -> compPrim n a =<< mapM (compTerm env) as

compAppLhs :: Env -> Term -> BB Val (Either (Value Val) (Value Closure))
compAppLhs env t =
  case t of
    Symbol qn -> Right `fmap` symbolClosure env qn
    _         -> Left  `fmap` compTerm env t

allocValBuffer :: Int32 -> BB Val (Value Val)
allocValBuffer len = alloca (fromLit len)

-- allocate enough space for vs, load them, call apply and return its result
compApp :: Env -> Term -> [Term] -> BB Val (Value Val)
compApp env c xs = do
  vs  <- mapM (compTerm env) xs
  fun <- compAppLhs env c
  case fun of
    Right clos -> apply    clos vs
    Left val   -> applyVal val  vs

markGC :: HasType a => Value (PtrTo a) -> BB Val ()
markGC ptr
  | not enableGC = return ()
  | otherwise    = do
    i8       <- bitcast ptr
    stackVar <- alloca
    store i8 stackVar
    call_ llvm_gcroot stackVar nullPtr

-- | Allocate a new closure that uses the given function symbol.
symbolClosure :: Env -> QualName -> BB Val (Value Closure)
symbolClosure env n = do
  (arity,fn) <- lookupCode n env
  clos       <- allocClosure (funAddr fn) (fromLit arity) nullPtr
  markGC clos
  return clos

-- | Interestingly, if this is the only entry point to argument, no dynamically
-- generated argument indexes can ever be used.
argument :: Env -> Nat -> BB Val (Value Val)
argument env = argsAt (envClosure env) . fromLit

-- | Pull a closure out of the environment, calling rts_barf if the value isn't
-- actually a closure.
argumentClosure :: Env -> Nat -> BB Val (Value Closure)
argumentClosure env i = do
  val <- argument env i
  ty  <- load =<< getValType val
  cmp <- icmp Ieq ty valClosure

  exit <- freshLabel
  barf <- freshLabel
  br cmp exit barf
  _    <- defineLabel barf (call_ rts_barf >> unreachable)
  clos <- defineLabel exit (getCval val)

  return clos

-- | Compile non-recursive let-declarations.
compLet :: Env -> [LetDecl] -> Term -> BB Val (Value Val)
compLet env0 ds body = flip compTerm body =<< foldM stepDecl env0 ds
  where
  stepDecl env d = do
    (n,v) <- compLetDecl env0 d
    return (addLocal n v env)

-- | Compile a let-declaration.
compLetDecl :: Env -> LetDecl -> BB Val (String,Value Val)
compLetDecl env d = name `fmap` compTerm env (letBody d)
  where
  name x = (letName d, x)

compVar :: Env -> Name -> BB Val (Value Val)
compVar env n =
  case lookupLocal n env of
    Just v  -> return v
    Nothing -> compSymbol env (simpleName n)

compSymbol :: Env -> QualName -> BB Val (Value Val)
compSymbol env qn = do
  clos <- symbolClosure env qn
  cval <- allocVal valClosure
  markGC cval
  setCval cval clos
  return cval

compLit :: AST.Literal -> BB Val (Value Val)
compLit (AST.LInt i) = do
  ival <- allocVal valInt
  markGC ival
  setIval ival (fromLit i)
  return ival

compPrim :: String -> Int -> [Value Val] -> BB Val (Value Val)
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
