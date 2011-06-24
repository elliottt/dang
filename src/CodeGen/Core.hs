module CodeGen.Core where

import CodeGen.Env
import CodeGen.Rts
import CodeGen.Types
import Interface
import Pretty
import QualName
import ReadWrite
import qualified LambdaLift  as LL
import qualified Syntax.AST  as AST

import Control.Monad (foldM)
import Data.Int (Int64)
import Text.LLVM


-- Declarations ----------------------------------------------------------------

-- | Generate code for a block of declarations, and return their interface as an
-- effect.
cgDecls :: Interface R -> [LL.Decl] -> LLVM (Interface RW)
cgDecls iface ds = do
  let this = declsInterface ds
      env  = iface `mergeInterfaces` this
  cgImports iface
  mapM_ (cgDecl env) ds
  return this

-- | Declare all external imports.
cgImports :: Interface R -> LLVM ()
cgImports  = mapM_ (uncurry declareExtern) . ifaceContents

-- | Produce an import declaration for an imported symbol.
declareExtern :: QualName -> FunDecl -> LLVM ()
declareExtern qn fd = do
  let sym = Symbol (funSymbol fd)
      obj = ptrT heapObjT
  _ <- declare obj sym (replicate (funArity fd) obj)
  _ <- declare obj (funUnpackSym sym) [obj]
  return ()

-- | Generate the interface that represents a group of declarations.
declsInterface :: [LL.Decl] -> Interface RW
declsInterface  = foldl step emptyInterface
  where
  step iface d = addFunDecl (LL.declName d) (declToFunDecl d) iface

-- | Produce a @FunDecl@ from a declaration.
declToFunDecl :: LL.Decl -> FunDecl
declToFunDecl d = FunDecl
  { funArity  = length (LL.declVars d)
  , funSymbol = mangle (LL.declName d)
  }

-- | Generate code for a declaration.
--
-- NOTE: The declaration passed in must be present in the interface that is
-- given.  The assumption is that the given interface is a merged one, that also
-- includes all dependencies for this module.
cgDecl :: Interface R -> LL.Decl -> LLVM ()
cgDecl iface decl = do
  let qn   = LL.declName decl
      sym  = Symbol (funSymbol fd)
      info = funInfoTable sym
      args = replicate (funArity fd) (ptrT heapObjT)
      fd   = case findFunDecl qn iface of
        Nothing -> error ("cgDecl: ``" ++ pretty qn ++ "'' not defined")
        Just x  -> x

  -- generate the unpacking function
  unpack <- defineUnpack (funArity fd) sym

  -- define the info table constant
  let arity = length args
  global info $ struct False
    [ natT  -: closureFun
      -- XXX need to calculate the real size of the closure
    , natT  -: (arity * 8)
    , natT  -: arity
    , codeT -: unpack
    ]

  -- generate the function body
  _ <- define' emptyFunAttrs (ptrT heapObjT) sym args $ \ clos -> do
    label (Ident "Entry")
    res <- cgTerm (emptyCGEnv iface clos) (LL.declBody decl)
    ret res

  return ()


-- Terms -----------------------------------------------------------------------

-- | Generate code, recursively, for a @Term@.
cgTerm :: CGEnv -> LL.Term -> BB (Typed Value)
cgTerm env tm = case tm of
  LL.Apply f xs  -> cgApply env f xs
  LL.Let ds b    -> cgLet env ds b
  LL.Symbol qn   -> cgSymbol env qn
  LL.Var n       -> cgVar env n
  LL.Argument i  -> cgArgument env i
  LL.Lit l       -> cgLiteral l
  LL.Prim n a ts -> cgPrim env n a ts

cgApply :: CGEnv -> LL.Term -> [LL.Term] -> BB (Typed Value)
cgApply env (LL.Symbol f) xs = cgApplySym env f xs
cgApply env f             xs = cgApplyGen env f xs

cgApplySym :: CGEnv -> QualName -> [LL.Term] -> BB (Typed Value)
cgApplySym env qn xs = case lookupFunDecl qn env of
  Nothing -> error ("cgApplySym: ``" ++ pretty qn ++ "'' not defined")
  Just fd ->
    let sym   = Symbol (funSymbol fd)
        arity = funArity fd
     in symbolApply arity (ptrT heapObjT) sym =<< mapM (cgTerm env) xs

cgApplyGen :: CGEnv -> LL.Term -> [LL.Term] -> BB (Typed Value)
cgApplyGen env f xs = do
  xs' <- mapM (cgTerm env) xs
  f'  <- cgTerm env f
  closApply f' xs'

-- | Generate code for the bindings, extend the environment with the new names,
-- and generate code for the body.
cgLet :: CGEnv -> [LL.LetDecl] -> LL.Term -> BB (Typed Value)
cgLet env ds b = do
  env' <- foldM cgLetDecl env ds
  cgTerm env' b

-- | Generate code for a name bound inside of a let, returning the extended
-- environment.
cgLetDecl :: CGEnv -> LL.LetDecl -> BB CGEnv
cgLetDecl env d = do
  res <- cgTerm env (LL.letBody d)
  return (addVar (LL.letName d) res env)

-- | Lookup the global name of a @QualName@.
cgSymbol :: CGEnv -> QualName -> BB (Typed Value)
cgSymbol env qn = case lookupSymbol qn env of
  Nothing -> error ("cgSymbol: ``" ++ pretty qn ++ "'' not defined.")
  Just tv -> return tv

-- | Lookup a locally-defined variable in the current environment.
cgVar :: CGEnv -> Name -> BB (Typed Value)
cgVar env n = case lookupVar n env of
  Nothing -> error ("cgVar: ``" ++ n ++ "'' is not defined")
  Just tv -> return tv

-- | Lookup a function argument in the current environment.
cgArgument :: CGEnv -> Int -> BB (Typed Value)
cgArgument env i = case lookupArgument i env of
  Nothing -> error ("cgArgument: no argument ``" ++ show i ++ "''")
  Just tv -> return tv

-- | Literals.
cgLiteral :: AST.Literal -> BB (Typed Value)
cgLiteral (AST.LInt i) = cgBoxInt i

cgBoxInt :: Int64 -> BB (Typed Value)
cgBoxInt i = do
  obj     <- allocData (ptrT dataT -: Symbol "Int_info")
  payload <- heapObjPayloadPtr obj
  store i =<< bitcast payload (ptrT (iT 64))
  return obj

cgPrim :: CGEnv -> String -> Int -> [LL.Term] -> BB (Typed Value)
cgPrim _env _n _arity _args = error "cgPrim"
