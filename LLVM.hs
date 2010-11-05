{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM where

import Pretty as Pretty

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Monoid (Monoid(..))
import MonadLib
import Numeric (showHex)


-- Doc Utilities ---------------------------------------------------------------

instance Monoid Doc where
  mempty  = Pretty.empty
  mappend = ($+$)


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


-- Basic Block Monad -----------------------------------------------------------

newtype BB r a = BB
  { unBB :: LLVM a
  } deriving (Functor,Applicative,Monad,MonadFix)

instance WriterM (BB r) Doc where
  put = BB . put

instance RunWriterM (BB r) Doc where
  collect m = BB (collect (unBB m))


-- Tagged Values ---------------------------------------------------------------

newtype Value a = Value Doc

instance Pretty (Value a) where
  pp _ (Value d) = d

valueType :: Value a -> a
valueType  = error "valueType"

toValue :: IsValue a => a -> Value a
toValue a = Value (ppr a)

ppWithType :: IsValue a => Value a -> Doc
ppWithType v = ppType (valueType v) <+> ppr v

class Pretty a => IsValue a where
  ppType :: a -> Doc

instance IsValue Int8 where
  ppType _ = text "i8"

instance IsValue Int16 where
  ppType _ = text "i16"

instance IsValue Int32 where
  ppType _ = text "i32"

instance IsValue Int64 where
  ppType _ = text "i64"

instance IsValue Float where
  ppType _ = text "float"

instance IsValue Double where
  ppType _ = text "double"


-- Pointers --------------------------------------------------------------------

data PtrTo a
  = PtrTo Integer
  | NullPtr

ptrType :: PtrTo a -> a
ptrType  = error "ptrType"

instance Pretty a => Pretty (PtrTo a) where
  pp _ (PtrTo i) = text "0x" <+> text (showHex i "")
  pp _ NullPtr   = text "null"

instance IsValue a => IsValue (PtrTo a) where
  ppType ptr = ppType (ptrType ptr) <> char '*'

-- | Construct a pointer to an arbitrary point in memory.
ptrTo :: IsValue a => Integer -> Value (PtrTo a)
ptrTo  = toValue . PtrTo

-- | Construct the null pointer.
nullPtr :: IsValue a => Value (PtrTo a)
nullPtr  = toValue NullPtr

-- | Allocate some memory on the stack.
alloca :: IsValue a => Int32 -> Maybe Int -> BB r (Result (PtrTo a))
alloca n mb = mfix $ \ res -> do
  put $ text "alloca" <+> ppType (ptrType (resultType res))
     <> comma <+> ppWithType (toValue n)
     <> maybe empty (\a -> comma <+> text "align" <+> int a) mb
  return Result


-- Variables -------------------------------------------------------------------

class FreshVar m where
  freshVar :: m (Value a)

instance FreshVar LLVM where
  freshVar = do
    n <- freshName "var"
    return (Value (char '%' <> text n))

instance FreshVar (BB r) where
  freshVar = BB freshVar


-- Results ---------------------------------------------------------------------

data Result a = Result

resultType :: Result a -> a
resultType  = error "resultType"

retVoid :: BB () ()
retVoid  = put (text "ret void")

ret :: IsValue r => Value r -> BB r ()
ret v = put (text "ret" <+> ppType (valueType v) <+> ppr v)

observe :: IsValue a => BB r (Result a) -> BB r (Value a)
observe m = do
  (Result,expr) <- collect m
  res           <- freshVar
  put (ppr res <+> char '=' <+> expr)
  return res


-- Functions -------------------------------------------------------------------

newtype Fun f = Fun String

instance Pretty (Fun f) where
  pp _ (Fun s) = char '@' <> text s

instance IsFun f => IsValue (Fun f) where
  ppType fun = res <+> parens (commas args)
    where (args,res) = funParts (funType fun)

class IsFun f where
  funParts :: f -> ([Doc],Doc)

instance IsValue a => IsFun (IO a) where
  funParts io = ([], ppType (ioType io))

instance (IsValue a, IsFun b) => IsFun (a -> b) where
  funParts fun = (ppType (funHead fun) : b, r)
    where (b,r) = funParts (funTail fun)

class FunRet f r | f -> r
instance FunRet (IO a) a
instance (IsValue a, FunRet b r) => FunRet (a -> b) r

funRet :: FunRet f r => Fun f -> r
funRet  = error "funRet"

funType :: Fun f -> f
funType  = error "funType"

funHead :: (a -> b) -> a
funHead  = error "funHead"

funTail :: (a -> b) -> b
funTail  = error "funTail"

ioType :: IO a -> a
ioType  = error "ioType"

setFunType :: b -> Fun a -> Fun b
setFunType _ (Fun s) = Fun s


-- Function Calls --------------------------------------------------------------

class CallArgs f k | f -> k, k -> f where
  callArgs :: [Doc] -> Fun f -> k

instance IsValue a => CallArgs (IO a) (BB r (Result a)) where
  callArgs as fun = do
    let res  = ioType (funType fun)
    let args = reverse as
    put (text "call" <+> ppType res <+> ppr fun <> parens (commas args))
    return Result

instance (IsValue a, CallArgs b r) => CallArgs (a -> b) (Value a -> r) where
  callArgs as fun a = callArgs (arg:as) (setFunType (funTail f) fun)
    where
    f   = funType fun
    arg = ppWithType a

call :: CallArgs f k => Fun f -> k
call  = callArgs []


-- Function Declaration --------------------------------------------------------

declare :: IsFun f => Fun f -> LLVM ()
declare fun =
  put (text "declare" <+> res <+> ppr fun <> parens (commas args))
  where
  (args,res) = funParts (funType fun)


-- Function Definition ---------------------------------------------------------

class Define k f | k -> f, f -> k where
  defineBody :: k -> f -> LLVM ([Doc],Doc)

instance IsValue res => Define (BB res ()) (IO res) where
  defineBody m _ = do
    (_,body) <- collect (unBB m)
    return ([],body)

instance (IsValue a, Define k f) => Define (Value a -> k) (a -> f) where
  defineBody k f = do
    a         <- freshVar
    (as,body) <- defineBody (k a) (funTail f)
    return (ppWithType a:as,body)

define :: (IsFun f, Define k f) => String -> k -> LLVM (Fun f)
define n k = mfix $ \f' -> do
  let f       = Fun n `asTypeOf` f'
      (_,res) = funParts (funType f)
  (ps,body) <- defineBody k (funType f)
  put $ text "define" <+> res <+> ppr f <> parens (commas ps) <+> char '{'
    $+$ nest 2 body
    $+$ char '}'
  return f


-- Tests -----------------------------------------------------------------------

test1 :: Fun (Int32 -> IO Int32)
test1  = Fun "test1"

test2 :: Fun (Fun (Int32 -> IO Int32) -> Int8 -> IO Int32)
test2  = Fun "test2"

test3 = do
  id32 <- define "id32" $ \ x -> ret (x :: Value Int32)
  main <- define "main" $ do
    a <- observe $ call id32 (toValue 10)
    b <- observe $ call id32 a
    ret a

  return ()
