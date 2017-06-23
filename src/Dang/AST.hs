{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Dang.AST (
    module Dang.AST,
    Constraint
  ) where

import GHC.Exts (Constraint)


-- | The type of identifiers.
type family IdentOf syn :: *

-- | The type of types.
type family TypeOf syn :: *

-- | The type of type schemas.
type family SchemaOf syn :: *

-- | The type of metadata.
type family MetaOf syn :: *


type Cxt (f :: * -> Constraint) syn =
  ( f (IdentOf syn)
  , f (TypeOf syn)
  , f (SchemaOf syn)
  , f (MetaOf syn)
  )
