{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM where

import Pretty as Pretty

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Int (Int8,Int32,Int64)
import Data.Monoid (Monoid(..))
import MonadLib hiding (Label)
import Numeric (showHex)


-- Doc Utilities ---------------------------------------------------------------

instance Monoid Doc where
  mempty  = Pretty.empty
  mappend = ($+$)

-- | Add a line of output
emit :: WriterM m Doc => Doc -> m ()
emit  = put


-- Top-level LLVM Monad --------------------------------------------------------

newtype LLVM a = LLVM
  { unLLVM :: StateT Int (WriterT Doc Id) a
  } deriving (Functor,Applicative,Monad,MonadFix)

runLLVM :: LLVM a -> (a,Doc)
runLLVM (LLVM m) = (a,d)
  where
  ((a,_),d) = runId (runWriterT (runStateT 0 m))

instance WriterM LLVM Doc where
  put = LLVM . put

instance RunWriterM LLVM Doc where
  collect m = LLVM (collect (unLLVM m))

freshName :: String -> LLVM String
freshName pfx = LLVM $ do
  i <- get
  set $! i + 1
  return (pfx ++ show i)

-- Block-level LLVM Monad ------------------------------------------------------

newtype B r a = B
  { runB :: LLVM a
  } deriving (Functor,Applicative,Monad,MonadFix)

instance WriterM (B r) Doc where
  put = B . put

instance RunWriterM (B r) Doc where
  collect m = B (collect (runB m))


-- LLVM Types ------------------------------------------------------------------

-- | Types that have values.
data NonEmpty = NonEmpty

-- | Types that have no values associated with them.
data Empty = Empty

-- | Types that have an LLVM analogue.  The i parameter describes the number of
-- inhabitants in that type.  There are two acceptable types here, @NonEmpty@
-- for types that have values in LLVM, and @Empty@ for types that exist, but
-- don't have any values (like void).
class HasType a i | a -> i where
  ppType :: a -> Doc

instance HasType () Empty where
  ppType _ = text "void"

instance HasType Bool NonEmpty where
  ppType _ = text "i1"

instance HasType Int8 NonEmpty where
  ppType _ = text "i8"

instance HasType Int32 NonEmpty where
  ppType _ = text "i32"

instance HasType Int64 NonEmpty where
  ppType _ = text "i64"


-- Vars ------------------------------------------------------------------------

-- | A variable is a string identifier with a phantom type that represents its
-- type in the resulting LLVM.
newtype Var a = Var String

instance Pretty (Var a) where
  pp _ (Var s) = char '%' <> text s

-- | Extract the type of a variable.  This will throw an error if it is ever
-- evaluated.
varType :: Var a -> a
varType  = error "varType"

-- | Generate the @Doc@ that describes the definition of the variable.
varDecl :: HasType a i => Var a -> Doc
varDecl v = ppType (varType v) <+> ppr v


-- | Monads that will produce a fresh variable of the given type.
class FreshVar m where
  -- | Generate a fresh variable.
  freshVar :: HasType a i => m (Var a)

instance FreshVar LLVM where
  freshVar = Var `fmap` freshName "x"

instance FreshVar (B r) where
  freshVar = B freshVar


-- Literals --------------------------------------------------------------------

-- | Get the type associated with a value.  In literal cases, this is the
-- haskell type that is already associated with the value, but variables, for
-- example, have an underlying type, and are tagged as variables, thus the type
-- function to extract the underlying type.
class (Pretty a, HasType ty i) => GetType a ty i | a -> ty i where
  getType :: a -> ty
  getType  = error "getType"

instance GetType ()    ()    Empty
instance GetType Bool  Bool  NonEmpty
instance GetType Int8  Int8  NonEmpty
instance GetType Int32 Int32 NonEmpty
instance GetType Int64 Int64 NonEmpty
instance GetType a ty i => GetType (Var a) ty i


-- Pointers --------------------------------------------------------------------

data PtrTo a
  = PtrTo Int64
  | NullPtr

instance Pretty (PtrTo a) where
  pp _ (PtrTo addr) = text "0x" <> text (showHex addr "")
  pp _ NullPtr      = text "null"

-- | Extract the underlying type of the pointer.  This will throw an error if it
-- is ever evaluated.
ptrType :: PtrTo a -> a
ptrType  = error "ptrType"

nullPtr :: GetType a a i => PtrTo a
nullPtr  = NullPtr

instance HasType a i => HasType (PtrTo a) NonEmpty where
  ppType a = ppType (ptrType a) <> char '*'

instance (Pretty a, GetType a a i) => GetType (PtrTo a) (PtrTo a) NonEmpty


-- Return Values ---------------------------------------------------------------

-- | A result value.
data Result a = Result

-- | Return something returnable via the ``ret'' llvm instruction.  This fixes
-- the r parameter to the B monad, enforcing a return type across a function
-- definition.
ret :: (Pretty a, GetType a ty i) => a -> B ty ()
ret a = do
  emit (text "ret" <+> ppType (getType a) <+> ppr a)
  return ()


-- Observable Results ----------------------------------------------------------

-- | Observe something that produces a result, generating a fresh variable that
-- holds its value.
observe :: HasType a NonEmpty => B r (Result a) -> B r (Var a)
observe m = do
  (_,body) <- collect m
  res      <- freshVar
  emit (ppr res <+> char '=' <+> body)
  return res


-- Labels ----------------------------------------------------------------------

data Label = Label String

instance Pretty Label where
  pp _ (Label l) = text "label" <+> char '%' <> text l

newLabel :: B r Label
newLabel  = Label `fmap` B (freshName "L")

label :: Label -> B r a -> B r a
label (Label l) m = emit (text l <> char ':') >> m

goto :: Label -> B r ()
goto (Label l) = emit (text "br" <+> ppr l)

br :: (GetType a Bool i) => a -> Label -> Label -> B r ()
br c t f =
  emit (text "br i1" <+> ppr c <> comma <+> ppr t <> comma <+> ppr f)


-- Conditionals ----------------------------------------------------------------

icmpEq :: (GetType a ty NonEmpty, GetType b ty NonEmpty)
       => a -> b -> B r (Result Bool)
icmpEq a b = do
  let ty = getType a
  emit (text "icmp eq" <+> ppType ty <+> ppr a <> comma <+> ppr b)
  return Result


-- Argument Lists --------------------------------------------------------------

infixr 9 :>
data a :> b = a :> b

argHead :: a :> b -> a
argHead _ = error "argHead"

argTail :: a :> b -> b
argTail _ = error "argTail"

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

instance GetType a b i => ResultOf (Result a) b where
  resultOf _ = error "resultOf"

instance ResultOf b r => ResultOf (a :> b) r where
  resultOf _ = error "resultOf"


-- Argument Formatting ---------------------------------------------------------

newtype WithType a = WithType a

instance (Pretty a, GetType a ty NonEmpty)
  => Pretty (WithType a) where
  pp _ (WithType a) = ppType (getType a) <+> ppr a


-- Functions -------------------------------------------------------------------

newtype Fun args res = Fun
  { funSym    :: String
  }

funArgs :: Fun args res -> args
funArgs  = error "funArgs"

funResult :: Fun args res -> res
funResult  = error "funResult"

setArgs :: Fun a res -> b -> Fun b res
setArgs (Fun s) _ = Fun s

setResult :: Fun args a -> b -> Fun args b
setResult (Fun s) _ = Fun s

instance Pretty (Fun args res) where
  pp _ fun = char '@' <> text (funSym fun)


-- Function Definition ---------------------------------------------------------

-- | Walk down a use of :>, producing an argument list suitable for use in a
-- forward declaration, or function definition.
class FmtArgs a where
  fmtArgs :: a -> Doc

instance FmtArgs (Result a) where
  fmtArgs _ = empty

instance (HasType a i, FmtArgs b) => FmtArgs (Var a :> b) where
  fmtArgs (a :> b) = commaSep (varDecl a) (fmtArgs b)

-- | Arguments to the define function.
class FmtArgs ty => Define fun args ty | fun -> args ty, args -> fun where
  -- | Walk down the type of the function, generating a list of types with a
  -- result on the end.  Also, collect a list of fresh variable names that can
  -- be used during the output of the function.
  typeOf :: fun -> LLVM (args,ty)

  -- | Apply the structure of fresh names to the function that will define the
  -- body of the LLVM function.
  apply :: ty -> fun -> LLVM ()

instance Define (B a ()) () (Result a) where
  typeOf _  = return (error "HasArgs:args", Result)
  apply _ m = do
    _ <- runB m
    return ()

instance (HasType a i, Define b args ty)
  => Define (Var a -> b) (a :> args) (Var a :> ty) where
  typeOf k = do
    var      <- freshVar
    (args,r) <- typeOf (k var)
    return (error "HasArgs:args" :> args, var :> r)

  apply (a :> rest) fun = apply rest (fun a)


-- | Define an LLVM function.
define :: (Define fun args ty, ResultOf ty res, HasType res i)
       => String -> fun -> LLVM (Fun args res)
define n body = do
  (_args,t) <- typeOf body
  let res = resultOf t
  emit $  text "define" <+> ppType res
      <+> char '@' <> text n <> parens (fmtArgs t) <+> char '{'
  (_,d)    <- collect (apply t body)
  emit (nest 2 d)
  emit (char '}')
  return Fun { funSym = n }


-- Function Declaration --------------------------------------------------------

-- | Format the type of a function into a sequence of type/variable definitions.
class Declare a where
  fmtDeclare :: a -> Doc

instance Declare () where
  fmtDeclare _ = empty

instance (HasType h i, Declare tl) => Declare (h :> tl) where
  fmtDeclare a = commaSep (ppType (argHead a)) (fmtDeclare (argTail a))

-- | Declare an external function binding, given its type.
declare :: (Declare args, HasType res i) => Fun args res -> LLVM ()
declare fun =
  emit $  text "declare" <+> ppType (funResult fun)
      <+> ppr fun <+> parens (fmtDeclare (funArgs fun))


-- Function Calls --------------------------------------------------------------

-- | Arguments to the call function.
class CallArgs args res k | args -> res k where
  -- | Take a function symbol, and a list of already defined arguments that have
  -- been rendered, and produce a continuation that will use them to produce a
  -- function call.
  call' :: Fun args res -> [Doc] -> k

instance HasType res i => CallArgs () res (B res (Result res)) where
  call' fun ds = do
    let rty = funResult fun
    emit (text "call" <+> ppType rty <+> ppr (funSym fun) <> parens (commas ds))
    return Result

instance (GetType val ty NonEmpty, CallArgs args res k, HasType res NonEmpty)
  => CallArgs (ty :> args) res (val -> k) where
  call' fun ds val = do
    let _ :> args = funArgs fun
    call' (setArgs fun args) (ppr (WithType val) :ds)

-- | Given a function symbol, generate a call to it.
call :: CallArgs args res k => Fun args res -> k
call fun = call' fun []


-- Numeric Functions -----------------------------------------------------------

class HasType a NonEmpty => HasNum a where

instance HasNum Int8
instance HasNum Int32
instance HasNum Int64

mul :: (HasNum ty, GetType a ty NonEmpty, GetType b ty NonEmpty)
    => a -> b -> B r (Result ty)
mul x y = do
  let ty = getType x
  emit (text "mul" <+> ppType ty <+> ppr x <> comma <+> ppr y)
  return Result


-- Tests -----------------------------------------------------------------------

id32 :: Var Int32 -> B Int32 ()
id32 x = ret x

test3 :: LLVM (Fun (Int32 :> ()) Int32)
test3 = define "id32" id32

c32 :: Var Int32 -> Var Int32 -> B Int32 ()
c32 _x _y = ret (10 :: Int32)

malloc :: Fun (Int32 :> ()) (PtrTo Int8)
malloc  = Fun { funSym = "malloc" }

test4 :: LLVM (Fun (Int32 :> Int32 :> ()) Int32)
test4  = define "cond" (\x _y -> ret x)

test :: LLVM ()
test = do
  f <- define "f" id32
  _g <- define "g" c32

  _ <- define "main" $ do
    res <- observe (call f (0 :: Int32))
    ret res

  return ()


test2 :: LLVM ()
test2 = define "main" body >> return ()
  where
  body :: Var Int32 -> B Int32 ()
  body x = do
    t <- newLabel
    f <- newLabel

    b <- observe (icmpEq x (0 :: Int32))
    br b t f
    _ <- label t (ret (0 :: Int32))
    label f $ do
      a <- observe (mul x (10 :: Int32))
      ret a
