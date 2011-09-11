module TypeChecker.AST where

import Pretty
import QualName (QualName,simpleName)
import TypeChecker.Types (Type,Forall(..),forallData)
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

  ppList _ = semis . map ppr

-- | Typed variable introduction.
data Match
 = MTerm Term
 | MPat Pat Match
   deriving (Show)

instance FreeVars Match where
  freeVars (MTerm t)  = freeVars t
  freeVars (MPat p m) = freeVars m Set.\\ freeVars p

instance Pretty Match where
  pp _ (MTerm t)  = ppr t
  pp _ (MPat p m) = char '\\' <+> pp 1 p <+> text "->" <+> pp 0 m

ppMatch :: Match -> ([Doc],Doc)
ppMatch (MTerm t)  = ([],ppr t)
ppMatch (MPat p m) = (pp 1 p:as,b)
  where
  (as,b) = ppMatch m

data Pat
  = PVar Var
  | PWildcard
    deriving (Show)

instance FreeVars Pat where
  freeVars (PVar v)  = Set.singleton (simpleName v)
  freeVars PWildcard = Set.empty

instance Pretty Pat where
  pp _ (PVar v)  = text v
  pp _ PWildcard = char '_'

data Term
  = App Term [Type] [Term]
  | Let [Decl] Term
  | Local Var
  | Global QualName
  | Lit Literal
    deriving (Show)

instance FreeVars Term where
  freeVars (App t _ as) = freeVars (t:as)
  freeVars (Let ds t)   = (freeVars t `Set.union` freeVars ds)
                             Set.\\ Set.fromList (map declName ds)
  freeVars (Local v)    = Set.singleton (simpleName v)
  freeVars (Global n)   = Set.singleton n
  freeVars (Lit l)      = freeVars l

instance Pretty Term where
  pp p (App f ts xs) = optParens (p > 0) (pp 1 f <> ppTyApp ts <+> ppList 1 xs)
  pp p (Let ds e)    = optParens (p > 0)
                     $ text "let" <+> braces (semis (map ppr ds))
                   <+> text "in"  <+> ppr e
  pp _ (Local v)     = text v
  pp _ (Global qn)   = ppr qn
  pp _ (Lit l)       = ppr l
