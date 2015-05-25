{-# LANGUAGE RecordWildCards #-}

module Dang.TypeChecker.CheckTypes (
    tcModule
  ) where

import qualified Dang.Core.AST as Core
import           Dang.ModuleSystem.QualName (ppModName)
import           Dang.Monad (withLoc,logInfo)
import qualified Dang.Syntax.AST as Syn
import           Dang.TypeChecker.Monad
import           Dang.Utils.Location (Located,unLoc)
import           Dang.Utils.Pretty


tcModule :: Syn.Module -> TC Core.Module
tcModule Syn.Module { .. } =
  do logInfo (text "Checking module:" <+> ppModName (unLoc modName))
     decls' <- tcTopDecls modDecls
     return Core.Module { Core.modName = unLoc modName, Core.modDecls = decls' }


-- Top-level Declarations ------------------------------------------------------

tcTopDecls :: [Syn.TopDecl] -> TC Core.DeclGroup
tcTopDecls  = undefined
