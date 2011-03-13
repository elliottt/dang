{-# LANGUAGE DoRec #-}

module CodeGen.Rts where

import CodeGen.Types

import Control.Monad (zipWithM_)
import Data.Int (Int8,Int32)
import Data.List (genericLength,genericSplitAt)
import Text.LLVM


-- Garbage Collection Primitives -----------------------------------------------

type VoidPtr = PtrTo Int8

-- | Allocate some memory using the garbage collector.
gc_alloc :: Fun (Int32 -> Res VoidPtr)
gc_alloc  = simpleFun "allocate"

-- | Trigger a garbage collection.
gc_perform :: Fun (Res ())
gc_perform  = simpleFun "perform_gc"

-- | Cause the RTS to exit.
rts_barf :: Fun (Res ())
rts_barf  = simpleFun "barf"

-- | The memcpy intrinsic.
llvm_memcpy :: Fun (PtrTo Int8 -> PtrTo Int8 -> Int32 -> Int32 -> Bool
                    -> Res ())
llvm_memcpy  = simpleFun "llvm.memcpy.p0i8.p0i8.i32"

-- | The llvm.gcroot intrinsic
llvm_gcroot :: Fun (PtrTo (PtrTo Int8) -> PtrTo Int8 -> Res ())
llvm_gcroot  = simpleFun "llvm.gcroot"

-- | Declare all foreign imports.
rts_imports :: LLVM ()
rts_imports  = do
  declare gc_alloc
  declare gc_perform
  declare rts_barf
  declare llvm_memcpy
  declare llvm_gcroot


-- Runtime Primitives ----------------------------------------------------------

-- | Get the size of something, given a pointer to use as the base in a size
-- calculation.
sizeOf :: HasValues a => Value (PtrTo a) -> BB r (Value Nat)
sizeOf ptr1 = do
  comment "sizeOf"
  ptr2 <- getelementptr ptr1 (fromLit 1) []
  val1 <- ptrtoint ptr1
  val2 <- ptrtoint (ptr2 `asTypeOf` ptr1)
  sub val2 val1

-- | Generic allocation using the garbage collector.
allocate :: HasValues a => Value (PtrTo a) -> BB r (Value (PtrTo a))
allocate base = do
  comment "allocate"
  size <- sizeOf base
  ptr  <- call gc_alloc size
  bitcast ptr

-- | Copy some memory around.
memcpy :: HasValues a
       => Value (PtrTo a) -> Value (PtrTo a) -> Value Nat -> BB r ()
memcpy dst src len = do
  dstP <- bitcast dst
  srcP <- bitcast src
  call_ llvm_memcpy dstP srcP len (fromLit 8) (fromLit False)

-- | Pointer addition.
plusPtr :: HasValues a => Value (PtrTo a) -> Value Nat -> BB r (Value (PtrTo a))
plusPtr ptr i = inttoptr =<< add i =<< ptrtoint ptr

-- | Create a new value, with the given type.
allocVal :: Value ValType -> BB r (Value Val)
allocVal ty = do
  comment "allocVal"
  val <- allocate nullPtr
  setValType val ty
  return val

-- | Allocate a closure, and fill out its values.
allocClosure :: Value CodePtr -> Value Nat -> Value Args -> BB r (Value Closure)
allocClosure code arity env = do
  comment "allocClosure"
  clos <- allocate nullPtr
  store code  =<< closureCodePtr clos
  store arity =<< closureArity clos
  store env   =<< closureArgs clos
  return clos

-- | Copy the closure, filling out some additional arguments.
copyClosure :: Value Closure -> Value (PtrTo Val) -> Value Nat
            -> BB r (Value Closure)
copyClosure c vs extra = do
  comment "copyClosure"
  arity <- load =<< closureArity c
  code  <- load =<< closureCodePtr c
  env   <- load =<< closureArgs c
  allocClosure code arity =<< extendArgs env vs extra

-- | Allocate an Args.
allocArgs :: Value Nat -> BB r (Value Args)
allocArgs len = do
  comment "allocArgs"
  args    <- allocate nullPtr
  valSize <- sizeOf (nullPtr :: Value Val)
  env     <- bitcast =<< call gc_alloc =<< mul valSize len
  store env =<< argsPtr args
  store len =<< argsLen args
  return args

-- | Extend an Args, by allocating a fresh one with the additional arguments
-- filled out.
extendArgs :: Value Args -> Value (PtrTo Val) -> Value Nat -> BB r (Value Args)
extendArgs args0 vs extra = do
  comment "extendArgs"
  len0 <- load =<< argsLen args0
  vs0  <- load =<< argsPtr args0
  args <- allocArgs =<< add len0 extra
  env  <- load =<< argsPtr args
  memcpy env vs0 len0
  env' <- plusPtr env len0
  memcpy env' vs extra
  return args


-- Application -----------------------------------------------------------------

-- | Int32 indexes for values in an array.
valueIxs :: [Value Int32]
valueIxs  = map fromLit [0 ..]

storeValueAt :: Value (PtrTo Val) -> Value Int32 -> Value Val -> BB r ()
storeValueAt arr ix v = store v =<< getelementptr arr ix []

extendClosure :: Value Closure -> [Value Val] -> BB r (Value Closure)
extendClosure clos args = do
  let len = fromLit (genericLength args)
  ptr <- alloca len
  zipWithM_ (storeValueAt ptr) valueIxs args
  copyClosure clos ptr len

type Apply a = Value Closure -> [Value Val] -> BB Val a

-- | Application where the number of arguments matches the arity.
fullApply :: Apply (Value Val)
fullApply clos args = do
  comment "fullApply"
  c' <- extendClosure clos args
  closureEval c'

-- | Under-application.
underApply :: Apply (Value Val)
underApply clos args = do
  comment "underApply"
  c2  <- extendClosure clos args
  res <- allocVal valClosure
  setCval res c2
  return res

-- | Over-application.
knownOverApply :: Nat -> Apply (Value Val)
knownOverApply arity clos args = do
  comment "overApply"
  let (as,bs) = genericSplitAt arity args
  val <- fullApply clos as
  applyVal val bs

-- | Application, when the arity is statically known.
knownApply :: Nat -> Apply (Value Val)
knownApply arity clos args = do
  case compare (genericLength args) arity of
    LT -> underApply clos args
    EQ -> fullApply clos args
    GT -> knownOverApply arity clos args

-- | Application, when the function is a value.
applyVal :: Value Val -> [Value Val] -> BB Val (Value Val)
applyVal val vs = do
  clos <- getCval val
  unknownApply clos vs

-- | Application, when the arity isn't statically known.
unknownApply :: Apply (Value Val)
unknownApply clos0 args = do
  comment "unknownApply"
  defineFreshLabel $ \ entry -> do

    -- seed argument length
    rec len <- return (fromLit (genericLength args))

        -- seed argument vector, and index into it
        argp0 <- alloca len
        zipWithM_ (storeValueAt argp0) valueIxs args

        check        <- freshLabel
        exactOrUnder <- freshLabel
        over         <- freshLabel
        under        <- freshLabel
        exact        <- freshLabel
        done         <- freshLabel

        -- figure out what sort of application to do
        -- clos - the current closure
        -- left - the number of arguments available
        -- arity- the arity of the current closure
        -- ix   - the pointer into the argument array
        (clos,left,arity,ix) <- defineLabel check $ do
          closC  <- phi clos0 entry [(closO,over)]
          leftC  <- phi len   entry [(leftO,over)]
          ixC    <- phi argp0 entry [(argpO,over)]
          arityC <- load =<< closureArity clos

          b <- icmp Iult arityC leftC
          br b over exactOrUnder

          return (closC,leftC,arityC,ixC)

        -- duplicate the closure, extending with as many arguments as it can
        -- take.
        -- evaluate the closure, and pull out a resulting closure.
        -- calculate the new number of arguments left
        -- increment the argument index
        -- jump back to the check label for another loop iteration
        comment "over-application"
        (closO,leftO,argpO) <- defineLabel over $ do
          c'    <- copyClosure clos ix arity
          res   <- closureEval c'
          clos' <- getCval res
          left' <- sub left arity
          ix'   <- plusPtr ix arity

          jump check

          return (clos', left', ix')

        -- duplicate the closure, extending with the available arguments.
        -- jump to either the application label, or the allocation label.
        closEU <- defineLabel exactOrUnder $ do
          c' <- copyClosure clos ix left
          b  <- icmp Ieq arity left
          br b exact under
          return c'

    -- duplicate the closure, extending with the arguments.
    -- jump to the code pointer.
    -- jump to the done label.
    comment "exact-application"
    exactVal <- defineLabel exact $ do
      res <- closureEval closEU
      jump done
      return res

    -- duplicate the closure, extending with the 
    comment "under-application"
    underVal <- defineLabel under $ do
      res <- allocVal valClosure
      setCval res closEU
      jump done
      return res

    -- phi together the results of either a full application, or an under
    -- application.
    defineLabel done (phi exactVal exact [(underVal,under)])
