module CodeGen.Rts where

import CodeGen.Types

import Control.Monad.Fix (mfix)
import Data.Int (Int8,Int32,Int64)
import Text.LLVM
import Text.LLVM.AST (ppModule)


-- Garbage Collection Primitives -----------------------------------------------

type VoidPtr = PtrTo Int8

-- | Allocate some memory using the garbage collector.
gc_alloc :: Fun (Int32 -> Res VoidPtr)
gc_alloc  = simpleFun "allocate"

-- | Trigger a garbage collection.
gc_perform :: Fun (Res ())
gc_perform  = simpleFun "perform_gc"

-- | Declare all foreign imports.
rts_imports :: LLVM ()
rts_imports  = do
  declare gc_alloc
  declare gc_perform


-- Runtime Primitives ----------------------------------------------------------

-- | Get the size of something, given a pointer to use as the base in a size
-- calculation.
sizeOf :: HasValues a => Value (PtrTo a) -> BB r (Value Nat)
sizeOf ptr1 = do
  ptr2 <- getelementptr ptr1 (fromLit 1) []
  val1 <- ptrtoint ptr1
  val2 <- ptrtoint (ptr2 `asTypeOf` ptr1)
  sub val2 val1

-- | Generic allocation using the garbage collector.
allocate :: HasValues a => Value (PtrTo a) -> BB r (Value (PtrTo a))
allocate base = do
  size <- sizeOf base
  ptr  <- call gc_alloc size
  bitcast ptr

-- | Create a new value, with the given type.
newVal :: Value ValType -> BB r (Value Val)
newVal ty = do
  val <- allocate nullPtr
  setValType val ty
  return val

-- | Allocate a closure, and fill out its values.
allocClosure :: Value CodePtr -> Value Nat -> Value Args -> BB r (Value Closure)
allocClosure code arity env = do
  clos <- allocate nullPtr
  store code  =<< closureCodePtr clos
  store arity =<< closureArity clos
  store env   =<< closureArgs clos
  return clos

-- | Allocate an Args.
allocArgs :: Value Nat -> BB r (Value Args)
allocArgs len = do
  args    <- allocate nullPtr
  valSize <- sizeOf (nullPtr :: Value Val)
  env     <- bitcast =<< call gc_alloc =<< mul valSize len
  store env =<< argsPtr args
  store len =<< argsLen args
  return args
