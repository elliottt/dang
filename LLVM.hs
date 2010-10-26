{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LLVM where

import Error
import Pretty as Pretty

import Control.Applicative (Applicative(..))
import Data.Int (Int8,Int32,Int64)
import Data.Monoid (Monoid(..))
import MonadLib


-- Doc Utilities ---------------------------------------------------------------

instance Monoid Doc where
  mempty  = Pretty.empty
  mappend = ($+$)

-- | Add a line of output
emit :: WriterM m Doc => Doc -> m ()
emit  = put


-- Top-level LLVM Monad --------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: StateT Int (WriterT Doc Lift) a
  } deriving (Functor,Applicative,Monad)

runLLVM :: LLVM a -> (a,Doc)
runLLVM (LLVM m) = (a,d)
  where
  ((a,_),d) = runLift (runWriterT (runStateT 0 m))

instance WriterM LLVM Doc where
  put = LLVM . put

instance RunWriterM LLVM Doc where
  collect m = LLVM (collect (unLLVM m))


-- Block-level LLVM Monad ------------------------------------------------------

newtype B a = B
  { runB :: LLVM a
  } deriving (Functor,Applicative,Monad)

instance WriterM B Doc where
  put = B . put

instance RunWriterM B Doc where
  collect m = B (collect (runB m))


-- LLVM Primitives -------------------------------------------------------------

i8 :: Doc
i8  = text "i8"

i32 :: Doc
i32  = text "i32"

i64 :: Doc
i64  = text "i64"


-- LLVM Types ------------------------------------------------------------------

class HasType a where
  ppType :: a -> Doc

instance HasType () where
  ppType _ = text "void"

instance HasType Int8 where
  ppType _ = i8

instance HasType Int32 where
  ppType _ = i32

instance HasType Int64 where
  ppType _ = i64


-- Vars ------------------------------------------------------------------------

newtype Var a = Var String

instance Pretty (Var a) where
  pp _ (Var s) = char '%' <> text s

instance HasType a => HasType (Var a) where
  ppType = ppType . varType

-- | Extract the type of a variable.  This will throw an error if it is ever
-- evaluated.
varType :: Var a -> a
varType  = error "varType"

-- | Generate the @Doc@ that describes the definition of the variable.
varDecl :: HasType a => Var a -> Doc
varDecl v = ppType (varType v) <+> ppr v


class FreshVar m where
  -- | Generate a fresh variable.
  freshVar :: HasType a => m (Var a)

instance FreshVar LLVM where
  freshVar = LLVM $ do
    i <- get
    set $! i + 1
    return (Var ("x" ++ show i))

instance FreshVar B where
  freshVar = B freshVar


-- Literals --------------------------------------------------------------------

-- | Get the type associated with a value.  In literal cases, this is the
-- haskell type that is already associated with the value, but variables, for
-- example, have an underlying type, and are tagged as variables, thus the type
-- function to extract the underlying type.
class HasType ty => GetType a ty | a -> ty where
  getType :: a -> ty
  getType  = error "getType"

instance GetType ()    ()
instance GetType Int8  Int8
instance GetType Int32 Int32
instance GetType Int64 Int64
instance GetType a a => GetType (Var a) a


-- Pointers --------------------------------------------------------------------

data PtrTo a = PtrTo

-- | Extract the underlying type of the pointer.  This will throw an error if it
-- is ever evaluated.
ptrType :: PtrTo a -> a
ptrType  = error "ptrType"

instance HasType a => HasType (PtrTo a) where
  ppType a = ppType (ptrType a) <> char '*'


-- Return Values ---------------------------------------------------------------

data Result a = Result

-- | Return something returnable via the ``ret'' llvm instruction.
ret :: (Pretty a, HasType a) => a -> B (Result a)
ret a = do
  emit (text "ret" <+> ppType a <+> ppr a)
  return Result

-- | Observe something that produces a result, generating a fresh variable that
-- holds its value.
observe :: HasType a => B (Result a) -> B (Var a)
observe m = do
  (_,body) <- collect m
  res      <- freshVar
  emit (ppr res <+> char '=' <+> body)
  return res


-- Argument Lists --------------------------------------------------------------

infixr 9 :>
data a :> b = a :> b

-- | Walk down a function description until the result type is found, then
-- return its phantom parameter.  For example:
--
--   Var Int32 :> Result (Var Int32)
--
-- represents the type of the function that takes a variable of type Int32, and
-- returns that same variable.  You need to walk through the :> to find the
-- Result type, and return its phantom type when producing a Fun type that
-- describes it.
class ResultOf ty res | ty -> res where
  resultOf :: ty -> res

instance GetType a b => ResultOf (Result a) b where
  resultOf _ = error "resultOf"

instance ResultOf b r => ResultOf (a :> b) r where
  resultOf _ = error "resultOf"


-- Argument Formatting ---------------------------------------------------------

newtype WithType a = WithType a

instance (HasType ty, Pretty a, GetType a ty) => Pretty (WithType a) where
  pp _ (WithType a) = ppType (getType a) <+> ppr a


-- Functions -------------------------------------------------------------------

data Fun args res = Fun
  { funSym    :: String
  , funArgs   :: args
  , funResult :: res
  }

instance Pretty (Fun args res) where
  pp _ fun = char '@' <> text (funSym fun)


-- Function Definition ---------------------------------------------------------

-- | Walk down a use of :>, producing an argument list suitable for use in a
-- forward declaration, or function definition.
class FmtArgs a where
  fmtArgs :: a -> Doc

instance FmtArgs (Result a) where
  fmtArgs _ = empty

instance (HasType a, FmtArgs b) => FmtArgs (Var a :> b) where
  fmtArgs (a :> b) = commaSep (varDecl a) (fmtArgs b)

-- | Arguments to the define function.
class FmtArgs ty => Define fun args ty | fun -> args ty where
  -- | Walk down the type of the function, generating a list of types with a
  -- result on the end.  Also, collect a list of fresh variable names that can
  -- be used during the output of the function.
  typeOf :: fun -> LLVM (args,ty)

  -- | Apply the structure of fresh names to the function that will define the
  -- body of the LLVM function.
  apply :: ty -> fun -> LLVM ()

instance Define (B (Result a)) () (Result a) where
  typeOf _  = return (error "HasArgs:args", Result)
  apply _ m = do
    _ <- runB m
    return ()

instance (HasType a, Define b args ty)
  => Define (Var a -> b) (a :> args) (Var a :> ty) where
  typeOf k = do
    var      <- freshVar
    (args,r) <- typeOf (k var)
    return (error "HasArgs:args" :> args, var :> r)

  apply (a :> rest) fun = apply rest (fun a)


-- | Define an LLVM function.
define :: (Define fun args ty, ResultOf ty res, HasType res)
       => String -> fun -> LLVM (Fun args res)
define n body = do
  (args,t) <- typeOf body
  let res = resultOf t
  emit $  text "define" <+> ppType res
      <+> char '@' <> text n <> parens (fmtArgs t) <+> char '{'
  (_,d)    <- collect (apply t body)
  emit (nest 2 d)
  emit (char '}')
  return Fun
    { funSym    = n
    , funArgs   = args
    , funResult = res
    }


-- Function Declaration --------------------------------------------------------

class Declare a where
  fmtDeclare :: a -> Doc

instance Declare () where
  fmtDeclare _ = empty

instance (HasType h, Declare tl) => Declare (h :> tl) where
  fmtDeclare (h :> tl) = commaSep (ppType h) (fmtDeclare tl)

-- | Declare an external function binding, given its type.
declare :: (Declare args, HasType res) => Fun args res -> LLVM ()
declare fun =
  emit $  text "declare" <+> ppType (funResult fun)
      <+> ppr fun <+> parens (fmtDeclare (funArgs fun))


-- Function Calls --------------------------------------------------------------

class HasType res => CallArgs args res k | args -> res k where
  call' :: Fun args res -> [Doc] -> k

instance HasType res => CallArgs () res (B (Result res)) where
  call' fun ds = do
    let rty = funResult fun
    emit (text "call" <+> ppType rty <+> ppr (funSym fun) <> parens (commas ds))
    return Result

instance (Pretty val, GetType val ty, CallArgs args res k, HasType res)
  => CallArgs (ty :> args) res (val -> k) where
  call' fun ds val = do
    let _ :> args = funArgs fun
    call' (fun { funArgs = args }) (ppr (WithType val) :ds)

-- | Given a function symbol, generate a call to it.
call :: CallArgs args res k => Fun args res -> k
call fun = call' fun []


-- Tests -----------------------------------------------------------------------

id32 :: Var Int32 -> B (Result (Var Int32))
id32 x = ret x

c32 :: Var Int32 -> Var Int32 -> B (Result Int32)
c32 x y = ret 10

malloc :: Fun (Int32 :> ()) (PtrTo Int8)
malloc  = Fun
  { funSym = "malloc"
  , funArgs = undefined :> ()
  , funResult = undefined
  }

test = do
  f <- define "f" id32
  g <- define "g" c32
  define "main" $ do
    res <- observe (call f (0 :: Int32))
    ret res
