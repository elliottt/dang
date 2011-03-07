module CodeGen.PrimOps where

import CodeGen.Rts
import CodeGen.Types

import Data.Int (Int64)
import Text.LLVM
import Text.LLVM.AST (ICmpOp(..))

-- | Lift an Int64 primitive over Vals.
intPrimBinop :: (Value Int64 -> Value Int64 -> BB r (Value Int64))
             -> Value Val -> Value Val
             -> BB r (Value Val)
intPrimBinop k a b = do
  aI  <- getIval a
  bI  <- getIval b
  res <- k aI bI
  val <- allocVal valInt
  setIval val res
  return val

-- | Absolute value over Vals.
primAbs :: Value Val -> BB r (Value Val)
primAbs a = do
  aI <- getIval a

  pos <- freshLabel
  neg <- freshLabel
  out <- freshLabel
  b   <- icmp Islt aI (fromLit 0)
  br b neg pos

  posOut <- defineLabel pos $ do
    jump out
    return a

  negOut <- defineLabel neg $ do
    val <- allocVal valInt
    setIval val =<< sub (fromLit 0) aI
    jump out
    return val

  defineLabel out (phi negOut neg [(posOut,pos)])
