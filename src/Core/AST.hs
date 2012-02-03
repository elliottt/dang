module Core.AST (
    module Core.AST
  , Var
  , Literal(..)
  , PrimType(..)
  , PrimTerm(..)
  ) where

import ModuleSystem.Export (Exported(..),Export(..))
import Pretty
import QualName (QualName,simpleName)
import Syntax.AST (Var,Literal(..),PrimType(..),PrimTerm(..))
import TypeChecker.Types (Type,Scheme,Forall(..),forallData,tarrow,TParam)
import Variables (FreeVars(freeVars),DefinesQualName(definedQualName))

import Data.List (nub)
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


-- Declarations ----------------------------------------------------------------

-- | Fully qualified declarations.
data Decl = Decl
  { declName   :: QualName
  , declExport :: Export
  , declBody   :: Forall Match
  } deriving (Show)

instance Exported Decl where
  exportSpec = declExport

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


-- Variable Introduction -------------------------------------------------------

-- | Typed variable introduction.
data Match
 = MTerm  Term  Type
 | MSplit Match Match
 | MPat   Pat   Match
 | MFail  Type
   deriving (Show)

instance FreeVars Match where
  freeVars m = case m of
    MTerm t _  -> freeVars t
    MSplit l r -> freeVars l `Set.union` freeVars r
    MPat p m'  -> freeVars m' Set.\\ freeVars p
    MFail _    -> Set.empty

instance Pretty Match where
  pp _ m = case m of
    MTerm t ty -> ppr t <+> text "::" <+> ppr ty

    MSplit l r ->
      let lArm = ppMatch l
          rArm = ppMatch r
          ppArm (as,b) = hsep as <+> text "->" <+> b
       in ppArm lArm $$ ppArm rArm

    MPat p m' -> char '\\' <+> ppr p <+> text "->" <+> ppr m'

    MFail _ -> empty

matchType :: Match -> Type
matchType m = case m of
  MTerm _ ty -> ty
  MSplit l _ -> matchType l
  MPat p m'  -> patType p `tarrow` matchType m'
  MFail ty   -> ty

-- | Pretty-print the arguments with precedence 1, and the body with precedence
-- 0.
ppMatch :: Match -> ([Doc],Doc)
ppMatch m = case m of
  MPat p m' -> let (as,b) = ppMatch m' in (pp 2 p:as, b)
  _         -> ([], ppr m)

isMTerm :: Match -> Bool
isMTerm MTerm{} = True
isMTerm _       = False

isMPat :: Match -> Bool
isMPat MPat{} = True
isMPat _      = False

-- | Apply a function on the @MTerm@ portion of a @Match@.
atMTerm :: (Match -> Match) -> (Match -> Match)
atMTerm k = loop
  where
  loop m = case m of
    MTerm _ _  -> k m
    MSplit l r -> MSplit (loop l) (loop r)
    MPat p m'  -> MPat p (loop m')
    MFail _    -> m


-- Variable Patterns -----------------------------------------------------------

data Pat
  = PVar Var Type
  | PCon QualName [Pat] Type
  | PWildcard Type
    deriving (Show)

patType :: Pat -> Type
patType p = case p of
  PVar _ ty    -> ty
  PCon _ _ ty  -> ty
  PWildcard ty -> ty

patVars :: Pat -> [Var]
patVars p = case p of
  PVar n _    -> [n]
  PCon _ ps _ -> nub (concatMap patVars ps)
  PWildcard _ -> []

instance FreeVars Pat where
  freeVars p = case p of
    PVar n _     -> Set.singleton (simpleName n)
    PCon qn ps _ -> Set.singleton qn `Set.union` freeVars ps
    PWildcard _  -> Set.empty

instance Pretty Pat where
  pp _ p = parens $ case p of
    PVar n ty     -> ppr n    <+> text "::"                       <+> ppr ty
    PCon qn ps ty -> ppr qn   <+> hsep (map ppr ps) <+> text "::" <+> ppr ty
    PWildcard ty  -> char '_' <+> text "::"                       <+> ppr ty

data Term
  = AppT Term [Type]
  | App Term [Term]
  | Case Term Match
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
  freeVars tm = case tm of
    AppT f _  -> freeVars f
    App t as  -> freeVars (t:as)
    Case e m  -> freeVars e `Set.union` freeVars m
    Let ds t  -> (freeVars t `Set.union` freeVars ds)
                     Set.\\ Set.fromList (map declName ds)
    Global qn -> Set.singleton qn
    Local n   -> Set.singleton (simpleName n)
    Lit l     -> freeVars l

instance Pretty Term where
  pp p tm = case tm of
    AppT f vs -> pp 1 f <> char '@' <> brackets (commas (map ppr vs))
    App f xs  -> optParens (p > 0) (ppr f <+> ppList 1 xs)
    Case e m  -> optParens (p > 0)
               $ text "case" <+> ppr e <+> text "of" $$ ppr m
    Let ds e  -> optParens (p > 0)
               $ text "let" <+> declBlock (map ppDecl ds)
             <+> text "in"  <+> ppr e
    Global qn -> ppr qn
    Local n   -> ppr n
    Lit l     -> ppr l
