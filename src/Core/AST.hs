module Core.AST (
    module Core.AST
  , Var
  , Literal(..)
  , PrimType(..)
  , PrimTerm(..)
  , Export(..)
  ) where

import Pretty
import QualName (QualName,simpleName)
import TypeChecker.Types (Type,Scheme,Forall(..),forallData,tarrow)
import Syntax.AST (Var,Literal(..),Export(..),PrimType(..),PrimTerm(..))
import Variables (FreeVars(freeVars),DefinesQualName(definedQualName))

import qualified Data.Set as Set


data Module = Module
  { modName      :: QualName
  , modPrimTypes :: [PrimType]
  , modPrimTerms :: [PrimTerm]
  , modDecls     :: [Decl]
  } deriving (Show)

emptyModule :: QualName -> Module
emptyModule qn = Module
  { modName      = qn
  , modPrimTypes = []
  , modPrimTerms = []
  , modDecls     = []
  }

instance Pretty Module where
  pp _ m = text "module" <+> ppr (modName m)
       $+$ declBlock decls
    where
    decls = map ppr (modPrimTypes m)
         ++ map ppr (modDecls m)

-- | Pretty print a list of type parameters for a type application/definition.
ppTyApp :: Pretty a => [a] -> Doc
ppTyApp [] = empty
ppTyApp ts = brackets (commas (map ppr ts))

-- | Fully qualified declarations.
data Decl = Decl
  { declName   :: QualName
  , declExport :: Export
  , declBody   :: Forall Match
  } deriving (Show)

instance DefinesQualName Decl where
  definedQualName = declName

instance FreeVars Decl where
  freeVars d = Set.delete (declName d) (freeVars (forallData (declBody d)))

instance Pretty Decl where
  pp _ d   = ppr (declExport d) <+> ppDecl d
  ppList _ = declBlock . map ppr

-- | Pretty-print a declaration without its export annotation.
ppDecl :: Decl -> Doc
ppDecl d = ppr (declName d) <+> ppTyApp ps <+> hsep as <+> char '=' <+> b
  where
  Forall ps body = declBody d
  (as,b)         = ppMatch body

hasArgs :: Decl -> Bool
hasArgs  = not . isMTerm . forallData . declBody

declType :: Decl -> Scheme
declType d = Forall ps (matchType m)
  where
  Forall ps m = declBody d


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

matchType :: Match -> Type
matchType m = case m of
  MTerm _ ty -> ty
  MPat p m'  -> patType p `tarrow` matchType m'

-- | Pretty-print the arguments with precedence 1, and the body with precedence
-- 0.
ppMatch :: Match -> ([Doc],Doc)
ppMatch (MTerm t ty) = ([],ppr t <+> text "::" <+> ppr ty)
ppMatch (MPat p m)   = (pp 1 p:as,b)
  where
  (as,b) = ppMatch m

isMTerm :: Match -> Bool
isMTerm MTerm{} = True
isMTerm _       = False

isMPat :: Match -> Bool
isMPat MPat{} = True
isMPat _      = False

data Pat
  = PVar Var Type
  | PWildcard Type
    deriving (Show)

patType :: Pat -> Type
patType p = case p of
  PVar _ ty    -> ty
  PWildcard ty -> ty

patVars :: Pat -> [Var]
patVars (PVar n _) = [n]
patVars _          = []

instance FreeVars Pat where
  freeVars (PVar n _)    = Set.singleton (simpleName n)
  freeVars (PWildcard _) = Set.empty

instance Pretty Pat where
  pp _ (PVar n ty)    = parens (ppr n    <+> text "::" <+> ppr ty)
  pp _ (PWildcard ty) = parens (char '_' <+> text "::" <+> ppr ty)

data Term
  = AppT Term [Type]
  | App Term [Term]
  | Let [Decl] Term
  | Global QualName
  | Local Var
  | Lit Literal
    deriving (Show)

appT :: Term -> [Type] -> Term
appT f [] = f
appT f ts = case f of
  AppT f' ts' -> AppT f' (ts' ++ ts)
  _           -> AppT f ts

app :: Term -> [Term] -> Term
app f [] = f
app f xs = case f of
  App f' xs' -> App f' (xs' ++ xs)
  _          -> App f xs

letIn :: [Decl] -> Term -> Term
letIn [] e = e
letIn ds e = Let ds e

instance FreeVars Term where
  freeVars (AppT f _)  = freeVars f
  freeVars (App t as)  = freeVars (t:as)
  freeVars (Let ds t)  = (freeVars t `Set.union` freeVars ds)
                            Set.\\ Set.fromList (map declName ds)
  freeVars (Global qn) = Set.singleton qn
  freeVars (Local n)   = Set.singleton (simpleName n)
  freeVars (Lit l)     = freeVars l

instance Pretty Term where
  pp _ (AppT f vs) = pp 1 f <> char '@' <> brackets (commas (map ppr vs))
  pp p (App f xs)  = optParens (p > 0) (ppr f <+> ppList 1 xs)
  pp p (Let ds e)  = optParens (p > 0)
                   $ text "let" <+> declBlock (map ppDecl ds)
                 <+> text "in"  <+> ppr e
  pp _ (Global qn) = ppr qn
  pp _ (Local n)   = ppr n
  pp _ (Lit l)     = ppr l
