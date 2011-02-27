module CodeGen.Types where

import Data.Int (Int32)
import Text.LLVM (fromLit,PtrTo,Value,Res,Fun)


-- RTS Types -------------------------------------------------------------------

type Nat = Int32

type Fn = Fun (Closure -> Res Val)

type Closure = PtrTo Int32

type Val = PtrTo Int32

type ValType = Int32

valInt, valClosure :: Value ValType
valInt     = fromLit 0x0
valClosure = fromLit 0x1
