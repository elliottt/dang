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

-- | Data-type info tables.
dataT :: Type
dataT  = Alias (Ident "DataT")

-- | Generate an info table that includes the generic info table header.
infoTable :: [Type] -> Type
infoTable t = Struct (natT:t)

-- | Cast a specific info table to a generic one.
toInfoTable :: Typed Value -> BB (Typed Value)
toInfoTable ptr = bitcast ptr (ptrT infoT)

-- | The closure type of functions.
closureFun :: Typed Value
closureFun  = natT -: int 1

closureData :: Typed Value
closureData  = natT -: int 2

-- | Add definitions for all internal types.
defineTypes :: LLVM ()
defineTypes  = do
  alias (Ident "Nat") (iT 64)

  alias (Ident "Code") $ ptrT $ FunTy (ptrT heapObjT) [ptrT heapObjT]

  alias (Ident "HeapObj") $ Struct
    [ ptrT infoT     -- info table pointer
    , Array 0 (iT 8) -- Payload
    ]

  -- generic info tables
  alias (Ident "InfoT") (infoTable [])

  -- function info-tables have their arity, and a pointer to the function that
  -- will unpack the payload in the heap.
  alias (Ident "FunT") $ infoTable
    [ natT       -- payload size
    , natT       -- function arity
    , ptrT codeT -- code pointer
    ]

  -- data info tables
  alias (Ident "DataT") $ infoTable
    [ natT -- payload size
    ]
