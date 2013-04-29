{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Desugar where

import Dang.Monad (Dang)
import Dang.IO (logStage,logInfo,logDebug)
import Pretty (pretty)
import QualName (Name)
import Syntax.AST (Module,Match(..),Pat(..),Term(..))

import Control.Applicative (Applicative,(<$>),(<*>))
import Control.Monad (mapAndUnzipM,(<=<))
import Data.Data (Data(toConstr))
import Data.Function (on)
import Data.Generics (gmapM,extM)
import Data.List (sort,groupBy)
import Data.Maybe (catMaybes)
import MonadLib
    (StateT,runStateT,set,get,BaseM(inBase),ReaderT,runReaderT,ask,local)
import qualified Data.Set as Set
import qualified Data.Map as Map


-- | Run the desugaring stage on a @Module@.
desugar :: Module -> Dang Module
desugar m = do
  logStage "desugar"
  m' <- runDesugar (desugarModule m)
  logInfo "desugar output:"
  logInfo (pretty m')
  logDebug (show m')
  return m'


-- Desugaring Monad ------------------------------------------------------------

newtype Desugar a = Desugar
  { unDesugar :: ReaderT RO (StateT RW Dang) a
  } deriving (Functor,Applicative,Monad)

instance BaseM Desugar Dang where
  inBase = Desugar . inBase

runDesugar :: Desugar a -> Dang a
runDesugar m = fst `fmap` runStateT emptyRW (runReaderT emptyRO (unDesugar m))

data RW = RW
  { rwIndex   :: !Int
  , rwAvoid   :: Set.Set Name
  } deriving Show

emptyRW :: RW
emptyRW  = RW
  { rwIndex   = 0
  , rwAvoid   = Set.empty
  }

data RO = RO
  { roRewrite :: Map.Map Name Name
  } deriving Show

emptyRO :: RO
emptyRO  = RO
  { roRewrite = Map.empty
  }

avoid :: Name -> Desugar ()
avoid n = Desugar $ do
  rw <- get
  set $! rw { rwAvoid = Set.insert n (rwAvoid rw) }

freshName :: Desugar Name
freshName  = Desugar $ do
  rw <- get
  let loop i
        | Set.member n (rwAvoid rw) = loop (i+1)
        | otherwise                 = (n,i)
        where
        n = 'p' : show i
      (name,i') = loop (rwIndex rw)

  set $! rw { rwIndex = i', rwAvoid = Set.insert name (rwAvoid rw) }

  return name

rewrite :: [Name] -> Name -> Desugar a -> Desugar a
rewrite from to k = Desugar $ do
  ro <- ask
  let renaming = Map.fromList (zip from (repeat to))
  local (ro { roRewrite = Map.union renaming (roRewrite ro) }) (unDesugar k)

rename :: Name -> Desugar Name
rename n = Desugar $ do
  ro <- ask
  case Map.lookup n (roRewrite ro) of
    Just n' -> return n'
    Nothing -> return n


-- Module Desugaring -----------------------------------------------------------

desugarModule :: Module -> Desugar Module
desugarModule  = elimPatterns <=< expandPatterns


-- Pattern Expansion -----------------------------------------------------------

expandPatterns :: Data a => a -> Desugar a
expandPatterns  = gmapM expandPatterns
           `extM` expandMatchPatterns

expandMatchPatterns :: Match -> Desugar Match
expandMatchPatterns m = case m of

  MSplit l r -> MSplit <$> expandMatchPatterns l <*> expandMatchPatterns r

  MPat p m' -> do
    (p',k) <- unnestPatterns p
    rest   <- expandMatchPatterns m'
    return (MPat p' (k rest))

  MGuard p v m' -> unnestGuard v p <*> expandMatchPatterns m'

  MTerm tm -> MTerm <$> expandPatterns tm

  MFail -> return MFail

-- | Translate constructor patterns into pattern guards over simple patterns.
unnestPatterns :: Pat -> Desugar (Pat, Match -> Match)
unnestPatterns p = case p of

  PCon _ _  -> do
    n <- freshName
    k <- unnestGuard n p
    return (PVar n, k)

  PVar n -> do
    avoid n
    return (p, id)

  PWildcard -> return (p, id)

-- | Translate a guard 
unnestGuard :: Name -> Pat -> Desugar (Match -> Match)
unnestGuard n p = case p of
  PCon qn ps -> do
    (ps',ks) <- mapAndUnzipM unnestPatterns ps
    let m = MGuard (PCon qn ps') n
    return (foldl (.) m ks)

  _ -> snd <$> unnestPatterns p


-- Pattern Elimination ---------------------------------------------------------

elimPatterns :: Data a => a -> Desugar a
elimPatterns  = gmapM elimPatterns
         `extM` rewriteTerm
         `extM` elimMatchPatterns

elimMatchPatterns :: Match -> Desugar Match
elimMatchPatterns m = case splitArms m of

  [m'] -> gmapM elimPatterns m'

  arms -> do
    let groups = groupBy ((==) `on` toConstr) (sort arms)
    logInfo (show (map toConstr arms))
    foldArms `fmap` mapM optMatchGroup groups

splitArms :: Match -> [Match]
splitArms m = case m of
  MSplit l r -> l : splitArms r
  _          -> [m]

foldArms :: [Match] -> Match
foldArms []   = MFail
foldArms arms = foldl1 MSplit arms

optMatchGroup :: [Match] -> Desugar Match
optMatchGroup group = case group of

  -- fold up the singleton case
  [m] -> return m

  -- remove common patterns
  MPat (PVar n) body:ms -> do
    let step m = case m of
          MPat (PVar n') body' -> return (Just n',body')
          MPat PWildcard body' -> return (Nothing,body')
          _                    -> fail ("Unexpected constructor " ++ show group)
    (others,ms') <- mapAndUnzipM step ms
    logInfo ("renmame " ++ show others ++ " to " ++ n)
    MPat (PVar n) <$> rewrite (catMaybes others) n (elimMatchPatterns (foldArms (body:ms')))

  -- give up
  _ -> do
    logInfo "giving up"
    return (foldArms group)


rewritePat :: Pat -> Desugar Pat
rewritePat p = case p of
  PVar n     -> PVar <$> rename n
  PCon qn ps -> PCon qn <$> mapM rewritePat ps
  PWildcard  -> return p

rewriteTerm :: Term -> Desugar Term
rewriteTerm tm = case tm of
  Local n -> Local <$> rename n
  _       -> gmapM elimPatterns tm
