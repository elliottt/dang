{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Dang.TypeChecker.CheckKinds (
    kcType
  ) where

import           Dang.ModuleSystem.QualName
import           Dang.Monad (logInfo,addErr,withLoc)
import           Dang.Syntax.AST
import           Dang.TypeChecker.Env (Env,addKinds,allKinds)
import           Dang.TypeChecker.Monad
import qualified Dang.TypeChecker.Types as T
import           Dang.Utils.Location (Located,unLoc)
import           Dang.Utils.Pretty
import           Dang.Variables (boundVars,scc)

import           Control.Lens (view)
import           Control.Monad (replicateM,mzero)
import qualified Data.Foldable as F
import qualified Data.Set as Set


-- | Check that the type has the given kind.
kcType :: Type -> T.Kind -> TC ()

kcType (TFun f x) k =
  do xk <- freshVar T.kSet
     kcType f (T.kArrow xk k)
     kcType x xk

kcType (TCon n) k =
  do k' <- getKind n
     unify k' k

kcType (TLoc lt) k = withLoc lt $
     kcType (unLoc lt) k


-- Kind Embedding --------------------------------------------------------------

translateKind :: Kind -> TC T.Kind
translateKind k = case k of
  TFun f x ->
    do f' <- translateKind f
       x' <- translateKind x
       return (T.tArrow f' x')

  TCon n    -> return (T.TCon n)
  TLoc lt   -> withLoc lt (translateKind (unLoc lt))
  TVar{}    -> invalid "variable"
  TApp{}    -> invalid "application" 
  TTuple{}  -> invalid "tuple"
  TRowExt{} -> invalid "row"
  TEmptyRow -> invalid "row"

  where

  invalid label =
    do addErr (text "unexpected kind" <+> text label <> char ':' <+> pp k)
       mzero
