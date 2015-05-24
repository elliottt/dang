{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Dang.TypeChecker.CheckKinds where

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


kcModule :: Module -> TC Module
kcModule Module { .. } =
  do logInfo (text "Kind checking module:" <+> ppModName (unLoc modName))
     (decls',env) <- kcTopDecls modDecls
     return Module { modDecls = decls', .. }


kcTopDecls :: [TopDecl] -> TC ([TopDecl],Env)
kcTopDecls  = go [] mempty . scc
  where
  go decls kinds [] =
    return (decls,kinds)

  go decls kinds (g:gs) =
    do let ds    = F.toList g
           names = Set.toList (declaredTypes ds)

       logInfo $ hang (text "checking kinds for group")
                    2 (vcat (map pp ds))

       -- seed the environment with kind variables, to start with
       kvars <- replicateM (length names) (freshVar T.kSet)
       let dks = addKinds (zip names kvars) mempty

       withEnv dks $
         do ds'  <- mapM kcTopDecl ds
            dks' <- applySubst dks

            logInfo $ hang (text "kinds:")
                         2 (vcat [ pp n <> char ':' <+> pp k
                                 | (n,k) <- allKinds dks' ])

            go (ds' ++ decls) (dks' `mappend` kinds) gs


-- | Filter out names that are declared at the type level.
declaredTypes :: [TopDecl] -> Set.Set Name
declaredTypes tds = Set.filter isTypeDecl (boundVars tds)
  where
  isTypeDecl n = view (qualName . qualLevel) n == Type 0


kcTopDecl :: TopDecl -> TC TopDecl
kcTopDecl (TDPrimType lpt) = TDPrimType `fmap` withLoc lpt (traverse kcPrimType lpt)


-- | Just assume the kind given by the programmer.
kcPrimType :: PrimType -> TC PrimType
kcPrimType pt @ PrimType { .. } =
  do logInfo (text "Checking:" <+> pp primTypeName)

     var <- getKind primTypeName
     sig <- translateKind primTypeKind
     unify var sig

     return pt


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
