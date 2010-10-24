{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LLVM where

import Error
import Pretty as Pretty

import Control.Applicative (Applicative(..))
import Data.Int (Int8,Int32,Int64)
import Data.Monoid (Monoid(..))
import MonadLib


instance Monoid Doc where
  mempty  = Pretty.empty
  mappend = ($+$)

newtype LLVM a = LLVM
  { unLLVM :: StateT Int (WriterT Doc Lift) a
  } deriving (Functor,Applicative,Monad)

runLLVM :: LLVM a -> (a,Doc)
runLLVM (LLVM m) = (a,d)
  where
  ((a,_),d) = runLift (runWriterT (runStateT 0 m))

-- | Add a line of output
emit :: Doc -> LLVM ()
emit  = LLVM . put

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

varType :: Var a -> a
varType  = error "varType"

varDecl :: HasType a => Var a -> Doc
varDecl v = ppType (varType v) <+> ppr v

-- | Generate a fresh variable.
freshVar :: HasType a => LLVM (Var a)
freshVar  = LLVM $ do
  i <- get
  set $! i + 1
  return (Var ("x" ++ show i))


-- Literals --------------------------------------------------------------------

class HasType ty => GetType a ty | a -> ty where
  getType :: a -> ty
  getType  = error "getType"

instance GetType Int8  Int8
instance GetType Int32 Int32
instance GetType Int64 Int64
instance GetType a a => GetType (Var a) a


-- Return Values ---------------------------------------------------------------

data Result a = Result

ret :: (Pretty a, HasType a) => a -> LLVM (Result a)
ret a = do
  emit (text "ret" <+> ppType a <+> ppr a)
  return Result

observe :: HasType a => LLVM (Result a) -> LLVM (Var a)
observe m = do
  (_,body) <- LLVM (collect (unLLVM m))
  res      <- freshVar
  emit (ppr res <+> char '=' <+> body)
  return res


-- Argument Lists --------------------------------------------------------------

infixr 9 :>
data a :> b = a :> b

-- | Walk down a use of :>, producing an argument list suitable for use in a
-- forward declaration, or function definition.
class FmtArgs a where
  fmtArgs :: a -> Doc

instance FmtArgs (Result a) where
  fmtArgs _ = empty

instance (HasType a, FmtArgs b) => FmtArgs (Var a :> b) where
  fmtArgs (a :> b) = commaSep (varDecl a) (fmtArgs b)

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

class FmtArgs ty => HasArgs fun args ty | fun -> args ty where
  typeOf :: fun -> LLVM (args,ty)
  apply  :: ty -> fun -> LLVM ()

instance HasArgs (LLVM (Result a)) () (Result a) where
  typeOf _  = return (error "HasArgs:args", Result)
  apply _ m = do
    _ <- m
    return ()

instance (HasType a, HasArgs b args ty)
  => HasArgs (Var a -> b) (a :> args) (Var a :> ty) where
  typeOf k = do
    var      <- freshVar
    (args,r) <- typeOf (k var)
    return (error "HasArgs:args" :> args, var :> r)

  apply (a :> rest) fun = apply rest (fun a)


define :: (HasArgs fun args ty, ResultOf ty res)
       => String -> fun -> LLVM (Fun args res)
define n body = do
  (args,t) <- typeOf body
  emit (text "define" <+> char '@' <> text n <> parens (fmtArgs t) <+> char '{')
  (_,d)    <- LLVM (collect (unLLVM (apply t body)))
  emit (nest 2 d)
  emit (char '}')
  return Fun
    { funSym    = n
    , funArgs   = args
    , funResult = resultOf t
    }


-- Function Calls --------------------------------------------------------------

class HasType res => CallArgs args res k | args -> res k where
  call' :: Fun args res -> [Doc] -> k

instance HasType res => CallArgs () res (LLVM (Result res)) where
  call' fun ds = do
    let rty = funResult fun
    emit (text "call" <+> ppType rty <+> ppr (funSym fun) <> parens (commas ds))
    return Result

instance (Pretty val, GetType val ty, CallArgs args res k, HasType res)
  => CallArgs (ty :> args) res (val -> k) where
  call' fun ds val = do
    let _ :> args = funArgs fun
    call' (fun { funArgs = args }) (ppr (WithType val) :ds)


call :: CallArgs args res k => Fun args res -> k
call fun = call' fun []


-- Tests -----------------------------------------------------------------------

id32 :: Var Int32 -> LLVM (Result (Var Int32))
id32 x = ret x

c32 :: Var Int32 -> Var Int32 -> LLVM (Result Int32)
c32 x y = ret 10

test = do
  f <- define "f" id32
  g <- define "g" c32
  define "main" $ do
    res <- observe (call f (0 :: Int32))
    ret res
