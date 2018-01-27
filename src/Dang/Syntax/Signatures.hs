{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Signatures (
    resolveSignatures
  ) where


import Dang.Monad
import Dang.Syntax.AST
import Dang.Utils.PP

import qualified Data.Map.Strict as Map


-- | Pair signatures with the declarations they describe. If any signatures
-- mention declarations that don't exist, or occur after declarations, errors
-- will be recorded and the pass will fail.
resolveSignatures :: PModule -> Dang PModule
resolveSignatures Module { .. } = failErrors $
  do resolved <- resolveDecls modDecls
     return Module { modDecls = resolved, .. }


-- Signature Resolution --------------------------------------------------------

type Resolve f = f Parsed -> Dang (f Parsed)

type SigEnv = Map.Map PName (Sig Parsed)

removeSig :: PName -> SigEnv -> Maybe (Sig Parsed, SigEnv)
removeSig name env =
  case Map.updateLookupWithKey (\ _ _ -> Nothing) name env of
    (Just sig, env') -> Just (sig, env')
    _                -> Nothing

-- | Associate signatures with bindings. Signatures may be placed before a
-- declaration, but will not associate to declarations placed behind them.
resolveDecls :: [Decl Parsed] -> Dang [Decl Parsed]
resolveDecls  = go [] Map.empty
  where

  -- no more declarations to process
  go acc sigs [] =
    do mapM_ sigWithoutBinding sigs
       return (reverse acc)

  -- add a signature to the signatures environment
  go acc sigs (DSig _ sig : ds) =
    go acc (Map.insert (sigName sig) sig sigs) ds

  -- for bindings, check to see if there's a signature to consume
  go acc sigs (DBind loc b : ds) =
    do b' <- resolveBind b
       case removeSig (bName b) sigs of
         Just (sig, sigs') ->
           go (DBind loc b' { bSig = Just (sigSchema sig) } : acc) sigs' ds

         Nothing ->
           go (DBind loc b' : acc) sigs ds


  -- recurse into module bindings
  go acc sigs (DModBind loc n mb : ds) =
    do mb' <- resolveModExpr mb
       go (DModBind loc n mb' : acc) sigs ds

  -- pass everything else through
  go acc sigs (d : ds) =
    go (d:acc) sigs ds


-- | Resolve signatures within a block of let declarations.
resolveLetDecls :: [LetDecl Parsed] -> Dang [LetDecl Parsed]
resolveLetDecls  = undefined


-- | Resolve signatures that occur within the body of a declaration.
resolveBind :: Resolve Bind
resolveBind Bind { .. } = withLoc bMeta $
  do body' <- resolveExpr bBody
     return Bind { bBody = body', .. }


-- | Resolve signatures within an expression.
resolveExpr :: Resolve Expr

resolveExpr e@EVar{} =
     return e

resolveExpr e@ECon{} =
     return e

resolveExpr e@ELit{} =
     return e

resolveExpr (EApp loc f xs) = withLoc loc $
     EApp loc <$> resolveExpr f <*> traverse resolveExpr xs

resolveExpr (EAbs loc m) = withLoc loc $
     EAbs loc <$> resolveMatch m

resolveExpr (ELet loc ds e) = withLoc loc $
     ELet loc <$> resolveLetDecls ds <*> resolveExpr e

resolveExpr (ECase loc body) = withLoc loc $
     ECase loc <$> resolveMatch body


-- | Resolve signatures within a matching expression.
resolveMatch :: Resolve Match
resolveMatch (MPat loc pat body) = withLoc loc $
     MPat loc pat <$> resolveMatch body

resolveMatch (MSplit loc l r) = withLoc loc $
     MSplit loc <$> resolveMatch l <*> resolveMatch r

resolveMatch m@MFail{} =
     return m

resolveMatch (MExpr loc body) = withLoc loc $
     MExpr loc <$> resolveExpr body


-- | Resolve signatures within a module expression.
resolveModExpr :: Resolve ModExpr
resolveModExpr  = undefined


-- Errors ----------------------------------------------------------------------

-- | Record errors for signatures that lack bindings.
sigWithoutBinding :: Sig Parsed -> Dang ()
sigWithoutBinding Sig { .. } =
  withLoc sigMeta $
  addError ErrNoDeclForSig $
  text "Missing value binding for signature" <+> quotes (pp sigName)
