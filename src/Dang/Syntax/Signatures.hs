{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Signatures (
    resolveSignatures
  ) where


import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.Utils.PP

import           Data.Either (partitionEithers)
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           MonadLib


-- | Pair signatures with the declarations they describe.
resolveSignatures :: PModule -> Dang PModule
resolveSignatures Module { .. } = failErrors $
  do let (sigs,others) = partitionSigs modDecls
     let (dups,env)    = processSigs sigs
     decls <- traverse (sigDecl env) others
     return Module { modDecls = decls, .. }


-- Environment -----------------------------------------------------------------

partitionSigs :: [Decl name] -> ([SrcLoc (Sig name)], [Decl name])
partitionSigs decls = partitionEithers (map checkSig decls)
  where
  checkSig d = go mempty d
    where
    go loc (DSig sig) = Left (sig `at` loc)
    go _   (DLoc loc) = go (locRange loc) (locValue loc)
    go _   _          = Right d

-- | Resolve signatures for a block of declarations.
sigBlock :: [Decl PName] -> Dang [Decl PName]
sigBlock decls =
  do let (sigs,others) = partitionSigs decls
     let (dups,env)    = processSigs sigs
     dupErrors dups
     traverse (sigDecl env) others


type Env = Map.Map PName (Schema PName)

type Dups = Map.Map PName (Set.Set SrcRange)

-- | Record errors from the duplicate environment.
dupErrors :: Dups -> Dang ()
dupErrors dups = mapM_ (uncurry duplicateSignatures) (Map.toList dups)

duplicateSignatures :: PName -> Set.Set SrcRange -> Dang ()
duplicateSignatures name dups
  | Set.size dups > 1 = addError ErrDuplicateSig msg
  | otherwise         = return ()
  where
  msg = text "There are multiple signatures for the declaration"
    <+> quotes (pp name)

processSigs :: [SrcLoc (Sig PName)] -> (Dups,Env)
processSigs  = foldl' go (mempty,mempty)
  where
  go st loc = foldl' addSig st (sigNames (thing loc))
    where
    schema = thing (sigSchema (thing loc))

    addSig (dups,sigs) Located { .. } =
      case Map.lookup locValue dups of
        Nothing -> ( Map.insert locValue (Set.singleton locRange) dups
                   , Map.insert locValue schema sigs )

        Just ns -> ( Map.adjust (Set.insert locRange) locValue dups, sigs )


-- Resolution ------------------------------------------------------------------

type ResolveSig f = Env -> f PName -> Dang (f PName)

sigDecl :: ResolveSig Decl
sigDecl env (DBind    bind)    = DBind <$> sigBind env bind
sigDecl _   (DSig     sig)     = error "unexpected DSig"
sigDecl _   d@DData{}          = pure d
sigDecl env (DModBind modBind) = DModBind <$> sigModBind env modBind
sigDecl _   d@DModType{}       = pure d
sigDecl env (DLoc     loc)     = DLoc <$> traverse (sigDecl env) loc

sigBind :: ResolveSig Bind
sigBind env b =
  case Map.lookup (thing (bName b)) env of
    Just sig -> return b { bSchema = Just sig }
    Nothing  -> return b

sigModBind :: ResolveSig ModBind
sigModBind  = error "sigModBind"
