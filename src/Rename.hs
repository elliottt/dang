{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Rename (
    Rename()
  , runRename
  , renameModule
  , renameDecls
  ) where

import QualName
import Syntax.AST

import Control.Applicative (Applicative(..),(<$>))
import MonadLib
import qualified Data.Map as Map


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

-- | Introduce a fresh name into a renaming context.
intro :: Monad m => (Var -> Rename m a) -> Rename m a
intro k = do
  i  <- get
  ro <- ask
  let (ro',i',[v]) = findFresh ro i [""]
  set i'
  local ro' (k v)

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

-- | Introduce term variable mappings between unqualified and qualified versions
-- of the same name.
fullyQualify :: Monad m => Namespace -> [Name] -> Rename m a -> Rename m a
fullyQualify ps ns m = do
  ro <- ask
  let step i n = addSubst n (qualName ps n) i
  local (foldl step ro ns) m



-- Term Renaming ---------------------------------------------------------------

renameModule :: Monad m => Module -> Rename m Module
renameModule m = fullyQualify (modNamespace m) names $ do
  ds' <- renameDecls (modDecls m)
  return m { modDecls = ds' }
  where
  names = map declName (modDecls m)

-- | Rename declarations, assuming that names are already present in the
-- environment.
renameDecls :: Monad m => [Decl] -> Rename m [Decl]
renameDecls  = mapM renameDecl

-- | Rename a declaration, assuming a fresh name is already in the environment.
renameDecl :: Monad m => Decl -> Rename m Decl
renameDecl d = do
  n' <- subst (declName d)
  fresh (declVars d) $ do
    vs' <- mapM subst (declVars d)
    b'  <- renameTerm (declBody d)
    return d
      { declName = n'
      , declVars = vs'
      , declBody = b'
      }

-- | Rename variable occurrences and bindings in terms.
renameTerm :: Monad m => Term -> Rename m Term
renameTerm t =
  case t of
    App f xs -> apply  <$> renameTerm f <*> mapM renameTerm xs
    Local v  -> Local  <$> subst v
    Lit l    -> Lit    <$> renameLiteral l
    Global n -> renameGlobal n
    Prim _   -> return t
    Let ds e ->
      fresh (map declName ds) (Let <$> mapM renameDecl ds <*> renameTerm e)
    Abs vs b -> intro $ \name -> fresh vs $ do
      vs' <- mapM subst vs
      b'  <- renameTerm b
      return (Let [Decl name vs' b'] (Local name))


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
