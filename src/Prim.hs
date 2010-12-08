module Prim where

type Primitive = (String,Int)

binary :: String -> Primitive
binary n = (n,2)

unary :: String -> Primitive
unary n = (n,1)

primitives :: [Primitive]
primitives  =
  -- integer functions
  [ binary "prim_add_i"
  , binary "prim_mul_i"
  , binary "prim_sub_i"
  , unary  "prim_abs_i"
  , unary  "prim_signum_i"
  ]
