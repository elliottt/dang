module TypeChecker.AST where

import Pretty
import QualName (QualName,simpleName)
import TypeChecker.Types (Type,Forall(..),forallData,TParam)
import Syntax.AST (Var,Literal)
import Variables (FreeVars(freeVars))

import qualified Data.Set as Set


-- | Pretty print a list of type parameters for a type application/definition.
ppTyApp :: Pretty a => [a] -> Doc
ppTyApp [] = empty
ppTyApp ts = brackets (commas (map ppr ts))

-- | Fully qualified declarations.
data Decl = Decl
  { declName   :: QualName
  , declBody   :: Forall Match
  } deriving (Show)

instance FreeVars Decl where
  freeVars d = Set.delete (declName d) (freeVars (forallData (declBody d)))

instance Pretty Decl where
  pp _ d = ppr (declName d) <+> ppTyApp ps <+> hsep as <+> char '=' <+> b
    where
    Forall ps body = declBody d
    (as,b)         = ppMatch body

  ppList _ = declBlock . map ppr

-- | Typed variable introduction.
data Match
 = MTerm Term Type
 | MPat Pat Match
   deriving (Show)

instance FreeVars Match where
  freeVars (MTerm t _) = freeVars t
  freeVars (MPat p m)  = freeVars m Set.\\ freeVars p

instance Pretty Match where
  pp _ (MTerm t ty)  = ppr t <+> text "::" <+> ppr ty
  pp _ (MPat p m) = char '\\' <+> pp 1 p <+> text "->" <+> pp 0 m

-- | Pretty-print the arguments with precedence 1, and the body with precedence
-- 0.
ppMatch :: Match -> ([Doc],Doc)
ppMatch (MTerm t ty) = ([],ppr t <+> text "::" <+> ppr ty)
ppMatch (MPat p m)   = (pp 1 p:as,b)
  where
  (as,b) = ppMatch m

data Pat
  = PVar Var Type
  | PWildcard Type
    deriving (Show)

instance FreeVars Pat where
  freeVars (PVar v _)    = Set.singleton (simpleName v)
  freeVars (PWildcard _) = Set.empty

instance Pretty Pat where
  pp _ (PVar v ty)    = parens (text v   <+> text "::" <+> ppr ty)
  pp _ (PWildcard ty) = parens (char '_' <+> text "::" <+> ppr ty)

data Term
  = AbsT [TParam] Term
  | AppT Term [Type]
  | App Term [Term]
  | Let [Decl] Term
  | Local Var
  | Global QualName
  | Lit Literal
    deriving (Show)

instance FreeVars Term where
  freeVars (AbsT _ b) = freeVars b
  freeVars (AppT f _) = freeVars f
  freeVars (App t as) = freeVars (t:as)
  freeVars (Let ds t) = (freeVars t `Set.union` freeVars ds)
                           Set.\\ Set.fromList (map declName ds)
  freeVars (Local v)  = Set.singleton (simpleName v)
  freeVars (Global n) = Set.singleton n
  freeVars (Lit l)    = freeVars l

instance Pretty Term where
  pp p (AbsT vs b) = optParens (p > 0)
                   $ text "\\@" <> hsep (map ppr vs) <+> text "->" <+> ppr b
  pp p (AppT f vs) = optParens (p > 0)
                   $ pp 1 f <> char '@' <> brackets (commas (map ppr vs))
  pp p (App f xs)  = optParens (p > 0) (ppr f <+> ppList 1 xs)
  pp p (Let ds e)  = optParens (p > 0)
                   $ text "let" <+> braces (semis (map ppr ds))
                 <+> text "in"  <+> ppr e
  pp _ (Local v)   = text v
  pp _ (Global qn) = ppr qn
  pp _ (Lit l)     = ppr l
