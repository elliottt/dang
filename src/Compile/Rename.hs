{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compile.Rename (
    rename
  ) where

import Dang.IO (logInfo,logStage,logDebug)
import Dang.Monad (Dang)
import Pretty (pretty)
import QualName
import Syntax.AST

import Control.Applicative (Applicative(..),(<$>))
import MonadLib
import qualified Data.Map as Map


-- External Interface ----------------------------------------------------------

rename :: Module -> Dang Module
rename m = do
  logStage "renamer"
  let m' = runLift (runRename [] (renameModule m))
  logInfo "Renaming output:"
  logDebug (show m')
  logInfo (pretty m')
  return m'


-- Renaming Monad --------------------------------------------------------------

data RO = RO
  { roSubst :: Map.Map Var QualName
  } deriving Show

addSubst :: Var -> QualName -> RO -> RO
addSubst a b ro = ro { roSubst = Map.insert a b (roSubst ro) }

captures :: RO -> Var -> Bool
captures ro v = Map.member v (roSubst ro)

newtype Rename m a = Rename
  { unRename :: StateT Int (ReaderT RO m) a
  } deriving (Functor,Applicative,Monad)

runRename :: Monad m => [Var] -> Rename m a -> m a
runRename bound (Rename m) = do
  let ro = RO
        { roSubst = Map.fromList [ (x,simpleName x) | x <- bound ]
        }
  (a,_) <- runReaderT ro (runStateT 0 m)
  return a

instance Monad m => StateM (Rename m) Int where
  get = Rename get
  set = Rename . set

instance Monad m => ReaderM (Rename m) RO where
  ask = Rename ask

instance Monad m => RunReaderM (Rename m) RO where
  local ro = Rename . local ro . unRename


subst' :: Monad m => Var -> Rename m QualName
subst' v = do
  ro <- ask
  case Map.lookup v (roSubst ro) of
    Nothing -> return (simpleName v)
    Just x  -> return x

subst :: Monad m => Var -> Rename m Var
subst v = do
  x <- subst' v
  return (qualSymbol x)

-- | Give fresh names to all that are passed, in the context of the given
-- action.
fresh :: Monad m => [Var] -> Rename m a -> Rename m a
fresh vs m = do
  i  <- get
  ro <- ask
  let (ro',i',_) = findFresh ro i vs
  set i'
  local ro' m

findFresh :: RO -> Int -> [Var] -> (RO,Int,[Var])
findFresh  = loop []
  where
  loop rs ro i []      = (ro, i,reverse rs)
  loop rs ro i (v:vs)
    | ro `captures` v' = loop     rs                 ro  i' (v:vs)
    | otherwise        = loop (v':rs) (addSubst v qn ro) i'    vs
    where
    i' = i + 1
    v' = "_" ++ v ++ show i
    qn = simpleName v'



-- Term Renaming ---------------------------------------------------------------

renameModule :: Monad m => Module -> Rename m Module
renameModule m = do
  ts' <- mapM renameTypedDecl (modTyped m)
  return m { modTyped = ts' }

-- | Rename a declaration, assuming a fresh name is already in the environment.
renameTypedDecl :: Monad m => TypedDecl -> Rename m TypedDecl
renameTypedDecl d = do
  n' <- subst (typedName d)
  m' <- renameMatch (typedBody d)
  return d
    { typedName = n'
    , typedBody = m'
    }

-- | Rename variable introductions.
renameMatch :: Monad m => Match -> Rename m Match
renameMatch m = case m of
  MTerm t   -> MTerm <$> renameTerm t
  MPat p m' -> fresh (patVars p) (MPat <$> renamePat p <*> renameMatch m')

-- | Rename the variables introduced by a pattern.
renamePat :: Monad m => Pat -> Rename m Pat
renamePat (PVar v)  = PVar <$> subst v
renamePat PWildcard = return PWildcard

-- | Rename variable occurrences and bindings in terms.
renameTerm :: Monad m => Term -> Rename m Term
renameTerm t =
  case t of
    App f xs -> apply  <$> renameTerm f <*> mapM renameTerm xs
    Local v  -> Local  <$> subst v
    Lit l    -> Lit    <$> renameLiteral l
    Global n -> renameGlobal n
    Let ts [] e -> fresh (map typedName ts)
                 $ Let <$> mapM renameTypedDecl ts <*> pure [] <*> renameTerm e
    Let _  _  _ ->
      fail "Unexpected untyped declarations in Let"

    Abs _ ->
      fail "Unexpected abstraction"

renameGlobal :: Monad m => QualName -> Rename m Term
renameGlobal qn
  | not (isSimpleName qn) = return (Global qn)
  | otherwise             = do
     qn' <- subst' (qualSymbol qn)
     if isSimpleName qn'
        then return (Local (qualSymbol qn'))
        else return (Global qn')


-- | Rename literals.  For the time being, this doesn't do anything.
renameLiteral :: Monad m => Literal -> Rename m Literal
renameLiteral (LInt i) = return (LInt i)