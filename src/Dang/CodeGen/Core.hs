{-# LANGUAGE Safe #-}

module CodeGen.Core where

import CodeGen.Env
import CodeGen.Rts
import CodeGen.Types
import Interface
import Pretty
import QualName
import ReadWrite
import qualified Compile.LambdaLift as LL
import qualified Syntax.AST as AST

import Control.Monad (foldM)
import Text.LLVM


-- Declarations ----------------------------------------------------------------

-- | Generate code for a block of declarations.
cgDecls :: InterfaceSet -> Interface RW -> [LL.Decl] -> LLVM ()
cgDecls iset this ds = do
  let env = addInterface this iset
  cgImports iset
  mapM_ (cgDecl env) ds

-- | Declare all external imports.
cgImports :: InterfaceSet -> LLVM ()
cgImports  = mapM_ (uncurry declareExtern) . funSymbols

-- | Produce an import declaration for an imported symbol.
declareExtern :: QualName -> FunSymbol -> LLVM ()
declareExtern _qn fd = do
  let sym = Symbol (funName fd)
      obj = ptrT heapObjT
  _ <- declare obj sym (replicate (funArity fd) obj)
  _ <- declare obj (funUnpackSym sym) [obj]
  return ()

-- | Generate code for a declaration.
--
-- NOTE: The declaration passed in must be present in the interface that is
-- given.  The assumption is that the given interface is a merged one, that also
-- includes all dependencies for this module.
cgDecl :: InterfaceSet -> LL.Decl -> LLVM ()
cgDecl iset decl = do
  let qn   = LL.declName decl
      sym  = Symbol (funName fd)
      info = funInfoTable sym
      args = replicate (funArity fd) (ptrT heapObjT)
      fd   = case lookupFunSymbol qn iset of
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
    res <- cgTerm (emptyCGEnv iset clos) (LL.declBody decl)
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

cgApply :: CGEnv -> LL.Term -> [LL.Term] -> BB (Typed Value)
cgApply env (LL.Symbol f) xs = cgApplySym env f xs
cgApply env f             xs = cgApplyGen env f xs

cgApplySym :: CGEnv -> QualName -> [LL.Term] -> BB (Typed Value)
cgApplySym env qn xs = case lookupFunSymbol qn env of
  Nothing -> error ("cgApplySym: ``" ++ pretty qn ++ "'' not defined")
  Just fd ->
    let sym   = Symbol (funName fd)
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

cgBoxInt :: Integer -> BB (Typed Value)
cgBoxInt i = do
  obj     <- allocData (ptrT dataT -: Symbol "Int_info")
  payload <- heapObjPayloadPtr obj
  store i =<< bitcast payload (ptrT (iT 64))
  return obj
