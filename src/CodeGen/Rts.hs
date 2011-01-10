module CodeGen.Rts where

import CodeGen.Types

import Data.Int (Int8,Int32,Int64)
import Text.LLVM (Fun,Res,declare,simpleFun,PtrTo,LLVM)


-- RTS Primitives --------------------------------------------------------------

rts_argument :: Fun (Closure -> Nat -> Res Val)
rts_argument  = simpleFun  "argument"

rts_apply :: Fun (Closure -> PtrTo Val -> Nat -> Res Val)
rts_apply  = simpleFun "apply"

rts_alloc_closure :: Fun (Nat -> PtrTo Fn -> Res Closure)
rts_alloc_closure  = simpleFun "alloc_closure"

rts_alloc_value :: Fun (ValType -> Res Val)
rts_alloc_value  = simpleFun "alloc_value"

rts_set_ival :: Fun (Val -> Int64 -> Res ())
rts_set_ival  = simpleFun "set_ival"

rts_set_cval :: Fun (Val -> Closure -> Res ())
rts_set_cval  = simpleFun "set_cval"

rts_get_ival :: Fun (Val -> Res Int64)
rts_get_ival  = simpleFun "get_ival"

rts_get_cval :: Fun (Val -> Res Closure)
rts_get_cval  = simpleFun "get_cval"

rts_value_type :: Fun (Val -> Res Int32)
rts_value_type  = simpleFun "value_type"

rts_barf :: Fun (Res ())
rts_barf  = simpleFun "barf"

llvm_gcroot :: Fun (PtrTo (PtrTo Int8) -> PtrTo Int8 -> Res ())
llvm_gcroot  = simpleFun "llvm.gcroot"

rtsImports :: LLVM ()
rtsImports  = do
  declare rts_argument
  declare rts_apply
  declare rts_alloc_closure
  declare rts_alloc_value
  declare rts_set_ival
  declare rts_get_ival
  declare rts_set_cval
  declare rts_get_cval
  declare rts_value_type
  declare rts_barf
  declare llvm_gcroot
