{-# LANGUAGE RecordWildCards #-}

module Dang.TypeCheck.KindCheck (checkModule) where

import Dang.ModuleSystem.Name (Name)
import Dang.Monad
import Dang.Syntax.AST
import Dang.TypeCheck.AST as TC
import Dang.TypeCheck.Monad
import Dang.Utils.Panic

import Data.List (partition)


checkModule :: HasCallStack => Module (Parsed Name) -> Dang (Module Checked)
checkModule m = runTC (kcModule m)


-- Checking --------------------------------------------------------------------

type KindCheck f = f (Parsed Name) -> TC (f Checked)

kcModule :: HasCallStack => KindCheck Module
kcModule Module { .. } = withLoc modMeta $
  do decls' <- kcStructDecls modDecls
     return Module { modDecls = decls', .. }

kcStructDecls :: HasCallStack => [Decl (Parsed Name)] -> TC [Decl Checked]
kcStructDecls ds =
  do let (sigs,rest) = partition isSig ds

     panic ("not done" ++ show sigs)
