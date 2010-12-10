module CodeGen.Types where

import Data.Int (Int32)
import Text.LLVM (IsType(getType),HasValues,toValue,PtrTo,Value,Fun,Res)


-- RTS Types -------------------------------------------------------------------

type Nat = Int32

type Fn = Fun (RtsEnv -> Res Val)


newtype RtsEnv = RtsEnv (PtrTo Int32)

instance IsType RtsEnv where
  getType (RtsEnv ptr) = getType ptr

instance HasValues RtsEnv


newtype Closure = Closure (PtrTo Int32)

instance IsType Closure where
  getType (Closure ptr)= getType ptr

instance HasValues Closure


newtype Val = Val (PtrTo Int32)

instance IsType Val where
  getType (Val ptr) = getType ptr

instance HasValues Val


type ValType = Int32

valInt, valClosure :: Value ValType
valInt     = toValue 0x0
valClosure = toValue 0x1


