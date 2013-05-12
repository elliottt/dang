{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}

module Dang.Core.AST (
    module Dang.Core.AST
  , Literal(..)
  , PrimType(..)
  , PrimTerm(..)
  ) where

import Dang.ModuleSystem.Export (Exported(..),Export(..))
import Dang.ModuleSystem.QualName (QualName,Name,simpleName)
import Dang.Syntax.AST (Literal(..),PrimType(..),PrimTerm(..))
import Dang.Traversal (Data,Typeable)
import Dang.TypeChecker.Types (Type,Forall(..),forallData,tarrow)
import Dang.Utils.Pretty
import Dang.Variables (FreeVars(freeVars),DefinesQualName(definedQualName))

import Data.List (nub)
import qualified Data.Set as Set


data Module = Module
  { modName      :: QualName
  , modPrimTypes :: [PrimType]
  , modPrimTerms :: [PrimTerm]
  , modDecls     :: [Decl]
  } deriving (Show,Data,Typeable)

emptyModule :: QualName -> Module
emptyModule qn = Module
  { modName      = qn
  , modPrimTypes = []
  , modPrimTerms = []
  , modDecls     = []
  }

instance Pretty Module where
  pp _ m = sep [ text "module" <+> ppr (modName m)
               , declBlock decls
               ]
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
  } deriving (Show,Data,Typeable)

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
ppDecl d = sep [ ppr (declName d) <+> ppTyApp ps
               , sep as <+> char '='
               , nest 2 b
               ]
  where
  Forall ps body = declBody d
  (as,b)         = ppMatch body

-- | Determine if a declaration has arguments.
hasArgs :: Decl -> Bool
hasArgs  = not . isMTerm . forallData . declBody

-- | Determine if a declaration is monomorphic.
isMono :: Decl -> Bool
isMono  = null . forallParams . declBody

-- | Compute the type of a declaration.
declType :: Decl -> Forall Type
declType d = Forall ps (matchType m)
  where
  Forall ps m = declBody d


-- Variable Introduction -------------------------------------------------------

-- | Typed variable introduction.
data Match
 = MTerm  Term  Type
 | MSplit Match Match
 | MPat   Pat   Match
 | MGuard Pat   Term  Type Match
 | MFail  Type
   deriving (Show,Data,Typeable)

instance FreeVars Match where
  freeVars m = case m of
    MTerm t _       -> freeVars t
    MSplit l r      -> freeVars l `Set.union` freeVars r
    MPat p m'       -> freeVars m' Set.\\ freeVars p
    MGuard p e _ m' -> (freeVars e `Set.union` freeVars m') Set.\\ freeVars p
    MFail _         -> Set.empty

instance Pretty Match where
  pp _ m = case m of
    MTerm t ty -> text "->" <+> ppr t <+> text "::" <+> ppr ty

    MSplit l r ->
      let lArm = ppMatch l
          rArm = ppMatch r
          ppArm (as,b) = hsep as <+> text "->" <+> b
       in ppArm lArm $$ ppArm rArm

    MPat p m' -> ppr p <+> ppr m'

    MGuard p e ty m' -> cat
      [ ppr p <+> text "<-" <+> parens (ppr e <+> ppr ty)
      , char ',' <+> ppr m'
      ]

    MFail _ -> empty

-- | The type of a match.
matchType :: Match -> Type
matchType m = case m of
  MTerm _ ty      -> ty
  MSplit l _      -> matchType l
  MPat p m'       -> patType p `tarrow` matchType m'
  MGuard _ _ _ m' -> matchType m'
  MFail ty        -> ty

-- | Pretty-print the arguments with precedence 1, and the body with precedence
-- 0.
ppMatch :: Match -> ([Doc],Doc)
ppMatch m = case m of
  MPat p m' -> let (as,b) = ppMatch m' in (pp 2 p:as, b)
  _         -> ([], ppr m)

ppCaseArms :: Match -> [Doc]
ppCaseArms m = case m of
  MSplit l r -> ppr l : ppCaseArms r
  _          -> [ppr m]

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
    MTerm _ _        -> k m
    MSplit l r       -> MSplit (loop l) (loop r)
    MPat p m'        -> MPat p (loop m')
    MGuard p e ty m' -> MGuard p e ty (loop m')
    MFail _          -> m


-- Variable Patterns -----------------------------------------------------------

data Pat
  = PVar Name Type
  | PCon QualName [Pat] Type
  | PWildcard Type
    deriving (Show,Data,Typeable)

patType :: Pat -> Type
patType p = case p of
  PVar _ ty    -> ty
  PCon _ _ ty  -> ty
  PWildcard ty -> ty

patVars :: Pat -> [Name]
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


-- Terms -----------------------------------------------------------------------

data Term
  = AppT Term [Type]
  | App Term [Term]
  | Case Term Match
  | Let [Decl] Term
  | Global QualName
  | Local Name
  | Lit Literal
    deriving (Show,Data,Typeable)

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
    Case e m  -> optParens (p > 0) (ppCase e m)
    Let ds e  -> optParens (p > 0)
               $ text "let" <+> declBlock (map ppDecl ds)
             <+> text "in"  <+> ppr e
    Global qn -> ppr qn
    Local n   -> ppr n
    Lit l     -> ppr l

ppCase :: Term -> Match -> Doc
ppCase e m = text "case" <+> ppr e <+> text "of"
          $$ cat (ppCaseArms m)
