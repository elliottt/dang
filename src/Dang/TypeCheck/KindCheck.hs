{-# LANGUAGE RecordWildCards #-}

module Dang.TypeCheck.KindCheck (checkModule) where

import Dang.ModuleSystem.Name (Name)
import Dang.Monad
import Dang.TypeCheck.AST as TC
import Dang.TypeCheck.Monad

import Dang.Syntax.AST


checkModule :: Module (Parsed Name) -> Dang (Module Checked)
checkModule m = runTC (kcModule m)


-- Checking --------------------------------------------------------------------

type KindCheck f = f (Parsed Name) -> TC (f Checked)

kcModule :: KindCheck Module
kcModule Module { .. } = withLoc modMeta $
  do decls' <- traverse kcDecl modDecls
     return Module { modDecls = decls', .. }

kcDecl :: KindCheck Decl
kcDecl  = undefined
