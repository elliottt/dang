{-# LANGUAGE StandaloneDeriving #-}

module AST where

import Data.Graph (SCC(..))
import Data.Graph.SCC (stronglyConnComp)
import qualified Data.Set as Set

class FreeVars a where
  freeVars :: a -> Set.Set Var

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = maybe Set.empty freeVars

instance FreeVars a => FreeVars [a] where
  freeVars = Set.unions . map freeVars


type Var = String

data Decl = Decl
  { declName     :: Var
  , declVars     :: [Var]
  , declExported :: Bool
  , declBody     :: Term
  } deriving Show

instance FreeVars Decl where
  freeVars d = Set.delete (declName d) (freeVars (declBody d))

declNames :: [Decl] -> [Var]
declNames  = map declName

deriving instance Show a => Show (SCC a)

sccDecls :: [Decl] -> [SCC Decl]
sccDecls  = stronglyConnComp . declsFvGraph

declsFvGraph :: [Decl] -> [(Decl,String,[String])]
declsFvGraph ds = graph
  where
  graph = [ (d, declName d, Set.toList (freeVars d)) | d <- ds ]


data Term
  = Abs [Var] Term
  | Let [Decl] Term
  | App Term Term
  | Var Var
  | Lit Literal
    deriving Show

instance FreeVars Term where
  freeVars (Abs vs t) = freeVars t Set.\\ Set.fromList vs
  freeVars (Let ds t) = Set.union (freeVars ds)
                      $ freeVars t Set.\\ Set.fromList (map declName ds)
  freeVars (App f x)  = Set.union (freeVars f) (freeVars x)
  freeVars (Lit l)    = freeVars l
  freeVars (Var x)    = Set.singleton x

-- | Collapse an abstraction into its arguments, and the body.
splitAbs :: Term -> ([Var],Term)
splitAbs t = loop t id
  where
  loop (Abs as b) f = loop b ((++ as) . f)
  loop b          f = (f [], b)


data Literal
  = LInt Int
    deriving Show

instance FreeVars Literal where
  freeVars _ = Set.empty
