module CodeGen.Types where

import Data.Int (Int32)
import Text.LLVM (toValue,PtrTo,Value,Fun,Res)


-- RTS Types -------------------------------------------------------------------

type Nat = Int32

type Fn = Fun (Closure -> Res Val)

type Closure = PtrTo Int32

type Val = PtrTo Int32

type ValType = Int32

valInt, valClosure :: Value ValType
valInt     = toValue 0x0
valClosure = toValue 0x1


