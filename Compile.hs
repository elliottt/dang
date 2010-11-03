{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compile where

import AST hiding (Var)
import Interface
import LLVM
import Pretty
import qualified AST

import Data.Int (Int32,Int64)
import Data.List (elemIndex)
import MonadLib


-- RTS Types -------------------------------------------------------------------

type Nat = Int32


data Closure = Closure

instance Pretty Closure where
  pp _ _ = error "pp Closure"

instance GetType Closure Closure NonEmpty where
  ppType _ = ppType (undefined :: PtrTo Int32)


data Value = Value

instance Pretty Value where
  pp _ _ = error "pp Value"

instance GetType Value Value NonEmpty where
  ppType _ = ppType (undefined :: PtrTo Int32)


data ValueType = ValueInt | ValueClosure
    deriving (Show)

instance Pretty ValueType where
  pp _ ValueInt     = int 0x0
  pp _ ValueClosure = int 0x1

-- The underlying type of ValueType is i32, but we're only going to make that
-- available when pretty printing, as there's no need for arithmetic operations
-- on the value tags.
instance GetType ValueType ValueType NonEmpty where
  ppType _ = ppType (undefined :: Int32)


-- RTS Primitives --------------------------------------------------------------

rts_argument :: Fun (Closure :> Nat :> ()) Value
rts_argument  = Fun
  { funSym     = "argument"
  , funLinkage = Nothing
  }

rts_apply :: Fun (Closure :> PtrTo Value :> Nat :> ()) Value
rts_apply  = Fun
  { funSym     = "apply"
  , funLinkage = Nothing
  }

rts_alloc_closure :: Fun () Closure
rts_alloc_closure  = Fun
  { funSym     = "alloc_closure"
  , funLinkage = Nothing
  }

rts_alloc_value :: Fun (ValueType :> ()) Value
rts_alloc_value  = Fun
  { funSym     = "alloc_value"
  , funLinkage = Nothing
  }

rts_set_ival :: Fun (Value :> Int64 :> ()) ()
rts_set_ival  = Fun
  { funSym     = "set_ival"
  , funLinkage = Nothing
  }

rts_set_cval :: Fun (Value :> Closure :> ()) ()
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

type Fn = Fun (Closure :> ()) Value

declLinkage :: Decl -> Maybe Linkage
declLinkage d
  | declExported d = Nothing
  | otherwise      = Just Private

lookupFn :: AST.Var -> Interface -> LLVM Fn
lookupFn n i =
  case findSymbol n i of
    Nothing -> (error "lookupFn: " ++ n)
    Just s  -> Fun
      { funName    = n
      , funLinkage = Nothing
      }

compModule :: [Decl] -> LLVM Interface
compModule ds = do
  let step (fns,i) d = do
        fn <- newFun (declLinkage d)
        let sym = Symbol (funSym fn)
        return (fn:fns, addSymbol (declName d) sym i)
  (fns0,i) <- foldM step ([],emptyInterface) ds
  zipWithM_ compDecl (reverse fns0) ds
  return i

compDecl :: Fn -> Decl -> LLVM ()
compDecl fn d = define fn $ \ env ->
  ret =<< compTerm (declVars d) env (declBody d)

compTerm :: [AST.Var] -> Var Closure -> Term -> B r (Var Value)
compTerm env rtsEnv t =
  case t of
    Abs vs b  -> error "compTerm: Abs"
    Let ds e  -> compLet env rtsEnv ds e
    App f xs  -> compApp env rtsEnv f xs
    AST.Var v -> compVar env rtsEnv v
    Lit l     -> compLit l

compLet :: [AST.Var] -> Var Closure -> [Decl] -> Term -> B r (Var Value)
compLet env rtsEnv ds e = do
  compDecls ds

compApp :: [AST.Var] -> Var Closure -> Term -> [Term] -> B r (Var Value)
compApp env rtsEnv f xs = undefined

compVar :: [AST.Var] -> Var Closure -> AST.Var -> B r (Var Value)
compVar env rtsEnv v = observe (call rts_argument rtsEnv idx)
  where
  idx :: Int32
  idx  = maybe (error "compVar") fromIntegral (elemIndex v env)

compLit :: Literal -> B r (Var Value)
compLit (LInt i) = do
  ival <- observe (call rts_alloc_value ValueInt)
  ignore (call rts_set_ival ival i)
  return ival
