{-# LANGUAGE TypeOperators #-}

module CodeGen.Types where

import Control.Monad ((<=<))
import Data.Int (Int8,Int32,Int64)
import Text.LLVM
import Text.LLVM.AST (ppModule)


-- RTS Types -------------------------------------------------------------------

type Nat = Int32


-- Values-----------------------------------------------------------------------

type ValType = Int32

valInt, valClosure :: Value ValType
valInt     = fromLit 0x0
valClosure = fromLit 0x1

type ValUnion = Struct (Int64 :> ())

type ValStruct = Struct (ValType :> ValUnion :> ())

type Val = PtrTo ValStruct

-- | Get a pointer to the union field.
valUnion :: Value Val -> BB r (Value (PtrTo ValUnion))
valUnion val = getelementptr val (fromLit 0) [fromLit 1]

-- | Get a pointer to the type field.
valType :: Value Val -> BB r (Value (PtrTo Int32))
valType val = getelementptr val (fromLit 0) [fromLit 0]

-- | Cast the union field of a value to an Int64.
ivalPtr :: Value (PtrTo ValUnion) -> BB r (Value (PtrTo Int64))
ivalPtr  = bitcast

-- | Cast the union field of a value to a Closure.
cvalPtr :: Value (PtrTo ValUnion) -> BB r (Value (PtrTo Closure))
cvalPtr  = bitcast

-- | Set the Int64 part of a value.
setIval :: Value Val -> Value Int64 -> BB r ()
setIval val ival = store ival =<< ivalPtr =<< valUnion val

-- | Get the Int64 part of a value.
getIval :: Value Val -> BB r (Value Int64)
getIval  = load <=< ivalPtr <=< valUnion

-- | Set the Closure part of a value.
setCval :: Value Val -> Value Closure -> BB r ()
setCval val cval = store cval =<< cvalPtr =<< valUnion val

-- | Get the Closure part of a value.
getCval :: Value Val -> BB r (Value Closure)
getCval  = load <=< cvalPtr <=< valUnion


-- Closures --------------------------------------------------------------------

type Code = Fun (PtrTo Args -> Res (PtrTo Val))

type CodePtr = PtrTo Code

type ClosureStruct = Struct (CodePtr :> Nat :> Args :> ())

type Closure = PtrTo ClosureStruct


-- Arguments -------------------------------------------------------------------

type ArgsStruct = Struct (PtrTo (PtrTo Val) :> Nat :> ())

type Args = PtrTo ArgsStruct

-- | Get the length of the arguments out of the args struct.
argsLen :: Value (PtrTo Args) -> BB r (Value Nat)
argsLen args = load =<< getelementptr args (fromLit 0) [fromLit 1]

-- | Get the value array out of the args struct.
argsPtr :: Value (PtrTo Args) -> BB r (Value (PtrTo (PtrTo Val)))
argsPtr args = load =<< getelementptr args (fromLit 0) [fromLit 0]

-- | Unchecked argument lookup.
argsAt :: Value (PtrTo Args) -> Value Nat -> BB r (Value (PtrTo Val))
argsAt args ix = do
  arr <- argsPtr args
  ptr <- getelementptr arr ix []
  load ptr
