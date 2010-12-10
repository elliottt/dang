module CodeGen.Rts where

import CodeGen.Types

import Data.Int (Int32,Int64)
import Text.LLVM (Fun,Res,declare,Fun(..),PtrTo,LLVM)


-- RTS Primitives --------------------------------------------------------------

rts_argument :: Fun (RtsEnv -> Nat -> Res Val)
rts_argument  = Fun
  { funSym     = "argument"
  , funLinkage = Nothing
  }

rts_apply :: Fun (Closure -> PtrTo Val -> Nat -> Res Val)
rts_apply  = Fun
  { funSym     = "apply"
  , funLinkage = Nothing
  }

rts_alloc_closure :: Fun (Nat -> PtrTo Fn -> Res Closure)
rts_alloc_closure  = Fun
  { funSym     = "alloc_closure"
  , funLinkage = Nothing
  }

rts_alloc_value :: Fun (ValType -> Res Val)
rts_alloc_value  = Fun
  { funSym     = "alloc_value"
  , funLinkage = Nothing
  }

rts_set_ival :: Fun (Val -> Int64 -> Res ())
rts_set_ival  = Fun
  { funSym     = "set_ival"
  , funLinkage = Nothing
  }

rts_set_cval :: Fun (Val -> Closure -> Res ())
rts_set_cval  = Fun
  { funSym     = "set_cval"
  , funLinkage = Nothing
  }

rts_get_ival :: Fun (Val -> Res Int64)
rts_get_ival  = Fun
  { funSym     = "get_ival"
  , funLinkage = Nothing
  }

rts_get_cval :: Fun (Val -> Res Closure)
rts_get_cval  = Fun
  { funSym     = "get_cval"
  , funLinkage = Nothing
  }

rts_get_type :: Fun (Val -> Res Int32)
rts_get_type  = Fun
  { funSym     = "get_type"
  , funLinkage = Nothing
  }

rts_barf :: Fun (Res ())
rts_barf  = Fun
  { funSym     = "barf"
  , funLinkage = Nothing
  }

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
  declare rts_get_type
  declare rts_barf


