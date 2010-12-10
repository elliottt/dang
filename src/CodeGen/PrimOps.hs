module CodeGen.PrimOps where

import CodeGen.Rts
import CodeGen.Types

import Data.Int (Int64)
import Text.LLVM

-- | Lift an Int64 primitive over Vals.
intPrimBinop :: (Value Int64 -> Value Int64 -> BB r (Value Int64))
             -> Value Val -> Value Val -> BB r (Value Val)
intPrimBinop k a b = do
  aI  <- call rts_get_ival a
  bI  <- call rts_get_ival b
  res <- k aI bI
  val <- call rts_alloc_value valInt
  call_ rts_set_ival val res
  return val

-- | Absolute value over Vals.
primAbs :: Value Val -> BB r (Value Val)
primAbs a = do
  aI <- call rts_get_ival a

  pos <- newLabel
  neg <- newLabel
  out <- newLabel
  b   <- icmp Ilt aI (toValue 0)
  condBr b neg pos

  posOut <- defineLabel pos $ do
    br out
    return a

  negOut <- defineLabel neg $ do
    val <- call rts_alloc_value valInt
    call_ rts_set_ival val =<< sub (toValue 0) aI
    br out
    return val

  defineLabel_ out (phi negOut posOut)
