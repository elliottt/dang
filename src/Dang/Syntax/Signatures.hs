{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Signatures (
    resolveSignatures
  ) where


import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.Utils.PP
import Dang.Utils.Panic

import           Data.Either (partitionEithers)
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           MonadLib


-- | Pair signatures with the declarations they describe.
resolveSignatures :: PModule -> Dang PModule
resolveSignatures Module { .. } = failErrors $
  do resolved <- resolveDecls modDecls
     return Module { modDecls = resolve }


-- Signature Resolution --------------------------------------------------------

type Resolve f = f Parsed -> Dang (f Parsed)

-- | Take signatures that are next to bindings, and associate them with the
-- bindings.
resolveDecls :: [Decl Parsed] -> Dang [Decl Parsed]
resolveDecls  = go [] Map.empty

  where

  go acc sigs [] = return (reverse acc)
  go acc sigs (DSig sig : rest) = go 
  go acc sigs (decl : rest)     = go (decl : acc) rest


  resolve 
