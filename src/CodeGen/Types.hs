module CodeGen.Types where

import Text.LLVM

-- | Natural numbers.
natT :: Type
natT  = Alias (Ident "Nat")

-- | Function pointers.
codeT :: Type
codeT  = Alias (Ident "Code")

-- | Heap-allocated objects.
heapObjT :: Type
heapObjT  = Alias (Ident "HeapObj")


-- Info Tables -----------------------------------------------------------------

-- | Generic info-table.
infoT :: Type
infoT  = Alias (Ident "InfoT")

-- | Function-type info tables.
funT :: Type
funT  = Alias (Ident "FunT")

-- | Generate an info table that includes the generic info table header.
infoTable :: [Type] -> Type
infoTable t = Struct (natT:t)

-- | Cast a specific info table to a generic one.
toInfoTable :: Typed Value -> BB (Typed Value)
toInfoTable ptr = bitcast ptr infoT

-- | The closure type of functions.
closureFun :: Typed Value
closureFun  = natT -: int 1

-- | Add definitions for all internal types.
defineTypes :: LLVM ()
defineTypes  = do
  alias (Ident "Nat") (iT 64)

  alias (Ident "Code") $ ptrT $ FunTy heapObjT [Array 0 heapObjT]

  alias (Ident "HeapObj") $ Struct
    [ ptrT infoT     -- info table pointer
    , Array 0 (iT 8) -- Payload
    ]

  alias (Ident "InfoT") (infoTable [])

  -- function info-tables have their arity, and a pointer to the function that
  -- will unpack the payload in the heap.
  alias (Ident "FunT") $ infoTable
    [ natT       -- function arity
    , ptrT codeT -- code pointer
    ]
