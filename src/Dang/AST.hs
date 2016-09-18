{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Dang.AST where

import GHC.Exts (Constraint)


-- | Generic parts of syntax.
class Syn syn where
  -- | The type of identifiers.
  type family IdentOf syn :: *

  -- | The type of types.
  type family TypeOf syn :: *

  -- | The type of type schemas.
  type family SchemaOf syn :: *

  -- | The type of metadata.
  type family MetaOf syn (tag :: k) :: *

-- | Apply this constraint to all of the generic parts of the syntax.
type All (f :: * -> Constraint) syn (xs :: [k]) =
  (f (IdentOf syn), f (TypeOf syn), f (SchemaOf syn), AllM f syn xs)

type family AllM (f :: * -> Constraint) syn (xs :: [k]) :: Constraint where
  AllM f syn '[]       = ()
  AllM f syn (x ': xs) = (f (MetaOf syn x), AllM f syn xs)

