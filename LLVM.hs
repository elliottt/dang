{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM (
    -- * Monadic Interface
    LLVM()
  , runLLVM
  , B()

    -- * Types
  , NonEmpty
  , Empty
  , GetType(ppType), getType
  , value

    -- ** Undefined Values
  , Undef()
  , undefType
  , undef

    -- ** Variables
  , Var()
  , varType
  , varDecl
  , FreshVar(freshVar)

    -- ** Pointers
  , PtrTo()
  , ptrType
  , nullPtr

    -- ** Array
  , Array()
  , arrayType

    -- * Return Values
  , Result()
  , ret
  , observe
  , ignore

    -- * Labels
  , Label()
  , newLabel
  , label
  , goto
  , br

    -- * Conditionals
  , icmpEq

    -- * Argument Lists
  , (:>)()
  , argHead
  , argTail
  , ResultOf(resultOf)

    -- * Functions
    -- ** Definition
  , Linkage(..)
  , Fun(..)
  , funArgs
  , funResult
  , Define()
  , newFun, newNamedFun
  , define, defineFun, defineNamedFun

    -- ** Declaration
  , Declare()
  , declare

    -- ** Calling
  , CallArgs()
  , call

    -- ** Numeric
  , HasInt(), HasFloat()
  , add, fadd
  , mul, fmul
  ) where

import Pretty as Pretty

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Int (Int8,Int16,Int32,Int64)
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



-- Literals --------------------------------------------------------------------

-- | Types that have values.
data NonEmpty = NonEmpty

-- | Types that have no values associated with them.
data Empty = Empty

-- | Get the type associated with a value.  In literal cases, this is the
-- haskell type that is already associated with the value, but variables, for
-- example, have an underlying type, and are tagged as variables, thus the type
-- function to extract the underlying type.
class Pretty a => GetType a ty i | a -> ty i where
  -- | Pretty-print the ty parameter.  a is used here, as it determines ty, the
  -- assumption being that instances will implement this as printing the type of
  -- a, and not printing a itself.
  ppType :: a -> Doc

getType :: GetType a ty i => a -> ty
getType  = error "getType"

instance GetType () () Empty where
  ppType _ = text "void"

instance GetType Bool  Bool  NonEmpty where
  ppType _ = text "i1"

instance GetType Int8  Int8  NonEmpty where
  ppType _ = text "i8"

instance GetType Int16 Int16 NonEmpty where
  ppType _ = text "i16"

instance GetType Int32 Int32 NonEmpty where
  ppType _ = text "i32"

instance GetType Int64 Int64 NonEmpty where
  ppType _ = text "i64"

instance GetType Float Float NonEmpty where
  ppType _ = text "float"

instance GetType Double Double NonEmpty where
  ppType _ = text "double"

value :: GetType a ty NonEmpty => a -> B r (Result ty)
value a = do
  emit (ppr a)
  return Result


-- Undefined Values ------------------------------------------------------------

data Undef a = Undef

instance Pretty (Undef a) where
  pp _ Undef = text "undef"

instance GetType a a NonEmpty => GetType (Undef a) a NonEmpty where
  ppType = ppType . undefType

undefType :: Undef a -> a
undefType  = error "undefType"

undef :: GetType a a NonEmpty => Undef a
undef  = Undef


-- Vars ------------------------------------------------------------------------

-- | A variable is a string identifier with a phantom type that represents its
-- type in the resulting LLVM.
newtype Var a = Var String

instance Pretty (Var a) where
  pp _ (Var s) = char '%' <> text s

instance GetType a a NonEmpty => GetType (Var a) a NonEmpty where
  ppType = ppType . varType


-- | Extract the type of a variable.  This will throw an error if it is ever
-- evaluated.
varType :: Var a -> a
varType  = error "varType"

-- | Generate the @Doc@ that describes the definition of the variable.
varDecl :: GetType a a NonEmpty => Var a -> Doc
varDecl v = ppType v <+> ppr v


-- | Monads that will produce a fresh variable of the given type.
class FreshVar m where
  -- | Generate a fresh variable.
  freshVar :: GetType a ty i => m (Var a)

instance FreshVar LLVM where
  freshVar = Var `fmap` freshName "x"

instance FreshVar (B r) where
  freshVar = B freshVar


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

instance (Pretty a, GetType a a i) => GetType (PtrTo a) (PtrTo a) NonEmpty where
  ppType a = ppType (ptrType a) <> char '*'


-- Arrays ----------------------------------------------------------------------

data Array a = Array Int32

instance Pretty a => Pretty (Array a) where
  pp _ _ = error "pp array?"

instance GetType a a NonEmpty => GetType (Array a) (Array a) NonEmpty where
  ppType a@(Array len) =
    brackets (ppr len <+> char 'x' <+> ppType (arrayType a))

arrayType :: Array a -> a
arrayType  = error "arrayType"


-- Return Values ---------------------------------------------------------------

-- | A result value.
data Result a = Result

resultType :: Result a -> a
resultType  = error "resultType"

-- | Return something returnable via the ``ret'' llvm instruction.  This fixes
-- the r parameter to the B monad, enforcing a return type across a function
-- definition.
ret :: (Pretty a, GetType a ty i) => a -> B ty ()
ret a = do
  emit (text "ret" <+> ppType a <+> ppr a)
  return ()


-- Observable Results ----------------------------------------------------------

-- | Observe something that produces a result, generating a fresh variable that
-- holds its value.
observe :: GetType a a NonEmpty => B r (Result a) -> B r (Var a)
observe m = do
  (_,body) <- collect m
  res      <- freshVar
  emit (ppr res <+> char '=' <+> body)
  return res

ignore :: B r (Result a) -> B r ()
ignore m = m >> return ()


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
  emit (text "icmp eq" <+> ppType a <+> ppr a <> comma <+> ppr b)
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

instance GetType a ty i => ResultOf (Result a) ty where
  resultOf _ = error "resultOf"

instance ResultOf b r => ResultOf (a :> b) r where
  resultOf _ = error "resultOf"


-- Argument Formatting ---------------------------------------------------------

newtype WithType a = WithType a

instance (Pretty a, GetType a ty NonEmpty) => Pretty (WithType a) where
  pp _ (WithType a) = ppType a <+> ppr a


-- Linkage Types ---------------------------------------------------------------

data Linkage
  = Private
  | LinkerPrivate
  | LinkerPrivateWeak
  | LinkerPrivateWeakDefAuto
  | Internal
  | AvailableExternally
  | Linkonce
  | Weak
  | Common
  | Appending
  | ExternWeak
  | LinkonceODR
  | WeakODR
  | DLLImport
  | DLLExport

instance Pretty Linkage where
  pp _ Private                  = text "private"
  pp _ LinkerPrivate            = text "linker_private"
  pp _ LinkerPrivateWeak        = text "linker_private_weak"
  pp _ LinkerPrivateWeakDefAuto = text "linker_private_weak_def_auto"
  pp _ Internal                 = text "internal"
  pp _ AvailableExternally      = text "available_externally"
  pp _ Linkonce                 = text "linkonce"
  pp _ Weak                     = text "weak"
  pp _ Common                   = text "common"
  pp _ Appending                = text "appending"
  pp _ ExternWeak               = text "extern_weak"
  pp _ LinkonceODR              = text "linkonce_ddr"
  pp _ WeakODR                  = text "weak_odr"
  pp _ DLLImport                = text "dllimport"
  pp _ DLLExport                = text "dllexport"


-- Functions -------------------------------------------------------------------

data Fun args res = Fun
  { funSym     :: String
  , funLinkage :: Maybe Linkage
  }

funArgs :: Fun args res -> args
funArgs  = error "funArgs"

funResult :: Fun args res -> res
funResult  = error "funResult"

setArgs :: Fun a res -> b -> Fun b res
setArgs (Fun s ml) _ = Fun s ml

setResult :: Fun args a -> b -> Fun args b
setResult (Fun s ml) _ = Fun s ml

instance Pretty (Fun args res) where
  pp _ fun = char '@' <> text (funSym fun)

class ArgList a where
  argList :: a -> Doc

instance ArgList () where
  argList _ = empty

instance (GetType h h i, ArgList tl) => ArgList (h :> tl) where
  argList l = commaSep (ppType (argHead l)) (argList (argTail l))

instance (ArgList args, GetType res res i)
  => GetType (Fun args res) (Fun args res) NonEmpty where
  ppType f = ppType (funResult f) <+> parens (argList (funArgs f))


-- Function Definition ---------------------------------------------------------

-- | Walk down a use of :>, producing an argument list suitable for use in a
-- forward declaration, or function definition.
class FmtArgs a where
  fmtArgs :: a -> Doc

instance FmtArgs (Result a) where
  fmtArgs _ = empty

instance (GetType a a NonEmpty, FmtArgs b) => FmtArgs (Var a :> b) where
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

instance (GetType a a NonEmpty, Define b args ty)
  => Define (Var a -> b) (a :> args) (Var a :> ty) where
  typeOf k = do
    var      <- freshVar
    (args,r) <- typeOf (k var)
    return (error "HasArgs:args" :> args, var :> r)

  apply (a :> rest) fun = apply rest (fun a)

-- | Create a new function symbol, with a fresh name.
newFun :: Maybe Linkage -> LLVM (Fun args res)
newFun ml = do
  sym <- freshName "f"
  return (Fun sym ml)

-- | Create a new function symbol with the provided name.
newNamedFun :: Maybe Linkage -> String -> LLVM (Fun args res)
newNamedFun ml sym = return Fun
  { funSym     = sym
  , funLinkage = ml
  }

-- | Define an LLVM function.
define :: (Define fun args ty, ResultOf ty res, GetType res res i)
       => Fun args res -> fun -> LLVM ()
define fun body = do
  (_args,t) <- typeOf body
  let res = resultOf t
  emit $ text "define" <+> ppr (funLinkage fun) <+> ppType res <+> ppr fun
      <> parens (fmtArgs t) <+> char '{'
  (_,d) <- collect (apply t body)
  emit (nest 2 d)
  emit (char '}')

-- | Define a function with a fresh name.
defineFun :: (Define fun args ty, ResultOf ty res, GetType res res i)
          => Maybe Linkage -> fun -> LLVM (Fun args res)
defineFun ml body = do
  fun <- newFun ml
  define fun body
  return fun

-- | Define a function with the given name.
defineNamedFun :: (Define fun args ty, ResultOf ty res, GetType res res i)
               => Maybe Linkage -> String -> fun -> LLVM (Fun args res)
defineNamedFun ml n body = do
  fun <- newNamedFun ml n
  define fun body
  return fun


-- Function Declaration --------------------------------------------------------

-- | Format the type of a function into a sequence of type/variable definitions.
class Declare a where
  fmtDeclare :: a -> Doc

instance Declare () where
  fmtDeclare _ = empty

instance (GetType h h i, Declare tl) => Declare (h :> tl) where
  fmtDeclare a = commaSep (ppType (argHead a)) (fmtDeclare (argTail a))

-- | Declare an external function binding, given its type.
declare :: (Declare args, GetType res res i) => Fun args res -> LLVM ()
declare fun =
  emit $  text "declare" <+> ppType (funResult fun)
      <+> ppr fun <+> parens (fmtDeclare (funArgs fun))


-- Function Calls --------------------------------------------------------------

-- | Arguments to the call function.
class CallArgs args res k | k -> args res where
  -- | Take a function symbol, and a list of already defined arguments that have
  -- been rendered, and produce a continuation that will use them to produce a
  -- function call.
  call' :: Fun args res -> [Doc] -> k

instance GetType res res i => CallArgs () res (B r (Result res)) where
  call' fun ds = do
    let rty = funResult fun
    emit $ text "call" <+> ppType rty <+> ppr fun
        <> parens (commas (reverse ds))
    return Result

instance (GetType a ty NonEmpty, CallArgs args res k)
  => CallArgs (ty :> args) res (a -> k) where
  call' fun ds val =
    call' (setArgs fun (argTail (funArgs fun))) (ppr (WithType val):ds)

-- | Given a function symbol, generate a call to it.
call :: CallArgs args res k => Fun args res -> k
call fun = call' fun []


-- Numeric Functions -----------------------------------------------------------

class HasInt ty

instance HasInt Int8
instance HasInt Int16
instance HasInt Int32
instance HasInt Int64

class HasFloat ty
instance HasFloat Float
instance HasFloat Double

add :: (HasInt ty, GetType a ty NonEmpty, GetType b ty NonEmpty)
    => a -> b -> B r (Result ty)
add x y = do
  emit (text "add" <+> ppType x <+> ppr x <> comma <+> ppr y)
  return Result

fadd :: (HasFloat ty, GetType a ty NonEmpty, GetType b ty NonEmpty)
     => a -> b -> B r (Result ty)
fadd x y = do
  emit (text "fadd" <+> ppType x <+> ppr x <> comma <+> ppr y)
  return Result

mul :: (HasInt ty, GetType a ty NonEmpty, GetType b ty NonEmpty)
    => a -> b -> B r (Result ty)
mul x y = do
  emit (text "mul" <+> ppType x <+> ppr x <> comma <+> ppr y)
  return Result

fmul :: (HasFloat ty, GetType a ty NonEmpty, GetType b ty NonEmpty)
     => a -> b -> B r (Result ty)
fmul x y = do
  emit (text "fmul" <+> ppType x <+> ppr x <> comma <+> ppr y)
  return Result


-- Stack Allocation ------------------------------------------------------------

alloca :: GetType a a NonEmpty => Int32 -> LLVM (Result (PtrTo a))
alloca len = mfix $ \ res -> do
  let ty = ptrType (resultType res)
  emit (text "alloca" <+> ppType ty <> comma <+> ppr (WithType len))
  return Result


-- Tests -----------------------------------------------------------------------

id' :: GetType a a NonEmpty => Var a -> B a ()
id' x = ret x

const' :: (GetType a a NonEmpty, GetType b b NonEmpty)
       => Var a -> Var b -> B a ()
const' a _ = ret a

test1 :: LLVM ()
test1 = do

  id32 <- defineNamedFun Nothing "id32" (\x -> ret (x :: Var Int32))

  _    <- defineNamedFun Nothing "main" $ do
    res <- observe (call id32 (0 :: Int32))
    ret res

  return ()

test2 :: LLVM ()
test2  = do

  id32  <- defineFun Nothing (\x -> ret (x :: Var Int32))
  _     <- defineNamedFun Nothing "main" $ do
    res <- observe (call id32 (0 :: Int32))
    ignore (call id32 res)
    ret res

  return ()
