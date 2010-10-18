{-# LANGUAGE StandaloneDeriving #-}

module AST where

import Pretty

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
  freeVars d = freeVars (declBody d) Set.\\ Set.fromList ns
    where ns = declName d : declVars d

instance Pretty Decl where
  pp _ d = text (declName d) <+> hsep (map text (declVars d)) <+>
           char '='          <+> pp 0 (declBody d)
  ppList _ ds = semis (map (pp 0) ds)

declNames :: [Decl] -> [Var]
declNames  = map declName

notExported :: Decl -> Decl
notExported d = d { declExported = False }

hasArguments :: Decl -> Bool
hasArguments  = not . null . declVars


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

instance Pretty Term where
  pp p t =
    case t of
      Abs vs b -> optParens (p > 0)
                $ char '\\' <> ppList 0 vs <> text "->" <> pp 0 b
      Let ds e -> optParens (p > 0)
                $ text "let" <+> braces (semis (map (pp 0) ds)) <+>
                  text "in"  <+> pp 0 e
      App f x  -> optParens (p > 0) (pp 1 f <+> pp 0 x)
      Var v    -> text v
      Lit l    -> pp 0 l


-- | Collapse an abstraction into its arguments, and the body.
splitAbs :: Term -> ([Var],Term)
splitAbs t = loop t id
  where
  loop (Abs as b) f = loop b ((++ as) . f)
  loop b          f = (f [], b)

lambda :: [Var] -> Term -> Term
lambda [] t = t
lambda as t = Abs as t

data Literal
  = LInt Int
    deriving Show

instance FreeVars Literal where
  freeVars _ = Set.empty

instance Pretty Literal where
  pp _ (LInt i) = int i
