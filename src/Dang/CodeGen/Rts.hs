{-# LANGUAGE DoRec #-}
{-# LANGUAGE Safe #-}

module CodeGen.Rts where

import CodeGen.Types
import QualName

import Text.LLVM


-- Utilities -------------------------------------------------------------------

type Offset = Typed Value -> BB (Typed Value)

offset :: Type -> Int -> Offset
offset ty off ptr = getelementptr (ptrT ty) ptr
  [ iT 32 -: int 0
  , iT 32 -: off
  ]

sizeof :: Type -> BB (Typed Value)
sizeof ty = do
  ptr <- getelementptr (ptrT ty) (nullPtr ty) [iT 32 -: int 1]
  ptrtoint ptr natT

note :: String -> BB a -> BB a
note n body = do
  comment ("begin " ++ n)
  res <- body
  comment ("end   " ++ n)
  return res


-- RTS Primitives --------------------------------------------------------------

gc_alloc :: Symbol
gc_alloc  = Symbol "gc_alloc"

gc_perform :: Symbol
gc_perform  = Symbol "gc_perform"

llvm_memcpy :: Symbol
llvm_memcpy  = Symbol "llvm.memcpy.p0i8.p0i8.i32"

definePrims :: LLVM ()
definePrims  = do
  -- garbage collector primitives
  declare (ptrT (iT 8)) gc_alloc   [natT]
  declare voidT         gc_perform []

  -- llvm intrinsics
  declare voidT llvm_memcpy [ ptrT (iT 8), ptrT (iT 8), iT 32, iT 32, iT 1 ]

  -- XXX remove this once integers are able to be defined in the language
  global (Symbol "Int_info") $ struct False
    [ natT -: closureData
    , natT -: int 8
    ]

-- | Convenient interface to the memcpy intrinsic.
memcpy :: Typed Value -> Typed Value -> Typed Value -> BB ()
memcpy dst src len = do
  len' <- trunc len (iT 32)
  call_ voidT llvm_memcpy [dst, src, len', iT 32 -: int 8, iT 1 -: int 0]


-- Heap Allocation -------------------------------------------------------------

-- | Generic heap allocation.  The info table pointer is not set here, but
-- instead is set in more specialized allocation functions.
heapAlloc :: Typed Value      -- ^ Payload Size
          -> BB (Typed Value) -- ^ Heap Object
heapAlloc size = "heapAlloc" `note` do
  it_sz <- sizeof (ptrT infoT)
  len   <- add it_sz size
  ptr   <- call (ptrT (iT 8)) gc_alloc [len]
  obj   <- bitcast ptr (ptrT heapObjT)
  return obj

-- | The address of the info table associated with a heap object.
heapObjInfoTablePtr :: Offset
heapObjInfoTablePtr  = offset (ptrT infoT) 0

-- | The address of the payload associated with a heap object.
heapObjPayloadPtr :: Offset
heapObjPayloadPtr  = offset (Array 0 (iT 8)) 1


-- Function Closures -----------------------------------------------------------

-- | Turn a function name into one that is suitable for code generation.
mangleName :: Arity -> QualName -> Symbol
mangleName _arity n = Symbol (mangle n)

-- | The name of the info table associated with a symbol.
funInfoTable :: Symbol -> Symbol
funInfoTable (Symbol n) = Symbol (n ++ "_info")

-- | The name of the function that will unpack a closure and jump to the
-- associated function.
funUnpackSym :: Symbol -> Symbol
funUnpackSym (Symbol n) = Symbol (n ++ "_unpack")

-- | Extract an argument from the closure at position i.  Note that the argument
-- is a pointer to the payload of an object in the heap, not the heap object
-- itself.
extractArg :: Typed Value -> Int -> BB (Typed Value)
extractArg clos i = getelementptr (ptrT heapObjT) clos [iT 32 -: i]

-- | Define the unpacking function for a function of a given arity.
defineUnpack :: Arity -> Symbol -> LLVM (Typed Value)
defineUnpack arity sym = do
  u <- define emptyFunAttrs (ptrT heapObjT) (funUnpackSym sym) (ptrT heapObjT)
    $ \ env -> do
      args <- case arity of
                0 -> return []
                _ -> do ptr  <- heapObjPayloadPtr env
                        clos <- bitcast ptr (ptrT heapObjT)
                        mapM (extractArg clos) [0 .. arity - 1]
      ret =<< call (ptrT heapObjT) sym args
  return (codeT -: u)

-- | The size of a function payload.
funTableSizePtr :: Offset
funTableSizePtr  = offset natT 1

-- | The arity pointer from a function info table.
funTableArityPtr :: Offset
funTableArityPtr  = offset natT 2

-- | The code pointer from a function info table.
funTableCodePtr :: Offset
funTableCodePtr  = offset codeT 3

-- | Allocate space for a function closure, given its info table.
allocFun :: Symbol           -- ^ Function Name
         -> BB (Typed Value) -- ^ Heap Object
allocFun sym = "allocFun" `note` do
  let itFun = funT -: funInfoTable sym
  obj <- heapAlloc =<< load =<< funTableSizePtr itFun
  it  <- toInfoTable itFun
  store it =<< heapObjInfoTablePtr obj
  return obj


-- Data Closures ---------------------------------------------------------------

-- | Allocate a 
allocData :: Typed Value      -- ^ Data Type Info Table
          -> BB (Typed Value) -- ^ Heap Object
allocData itData = "allocData" `note` do
  obj <- heapAlloc =<< load =<< dataTableSizePtr itData
  it  <- toInfoTable itData
  store it =<< heapObjInfoTablePtr obj
  return obj

dataTableSizePtr :: Offset
dataTableSizePtr  = offset natT 1


-- Application -----------------------------------------------------------------

type Arity = Int

-- | Application when the arity and function are known at compile time.
symbolApply :: Arity -> Type -> Symbol -> [Typed Value] -> BB (Typed Value)
symbolApply arity rty fun args = note "symbolApply" $
  case compare (length args) arity of
    LT -> symbolUnderApply fun args
    EQ -> call rty fun args
    GT -> symbolOverApply arity rty fun args

-- | Application when not enough arguments are present for execution to happen.
symbolUnderApply :: Symbol -> [Typed Value] -> BB (Typed Value)
symbolUnderApply fun _args = "symbolUnderApply" `note` do
  _obj <- allocFun fun
  undefined

symbolOverApply :: Arity -> Type -> Symbol -> [Typed Value] -> BB (Typed Value)
symbolOverApply arity rty sym args = "symbolOverApply" `note` do
  let (as,bs) = splitAt arity args
  clos <- call rty sym as
  closApply clos bs

-- | Application when all we have is a closure.
closApply :: Typed Value -> [Typed Value] -> BB (Typed Value)
closApply _clos _args = error "closApply"
