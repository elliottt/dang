{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Core.AST (
    module Dang.Core.AST
  , Literal(..)
  ) where

import Dang.ModuleSystem.Export (Exported(..),Export(..))
import Dang.ModuleSystem.QualName
import Dang.Syntax.AST ( Literal(..) )
import Dang.TypeChecker.Types
import Dang.Utils.Pretty
import Dang.Variables (FreeVars(freeVars),BoundVars(boundVars))

import           Data.Data ( Data )
import qualified Data.Set as Set
import           Data.Typeable ( Typeable )


data Module = Module
  { modName  :: ModName
  , modDecls :: [Decl]
  } deriving (Show,Data,Typeable)

emptyModule :: ModName -> Module
emptyModule qn = Module { modName = qn, modDecls = [] }

instance Pretty Module where
  ppr m = sep [ text "module" <+> pp (modName m)
              , layout (concatMap (ppDecl True) (modDecls m))
              ]

-- | Pretty print a list of type parameters for a type application/definition.
ppTyApp :: Pretty a => [a] -> PPDoc
ppTyApp [] = empty
ppTyApp ts = fsep (list (char '[') comma (char ']') (map pp ts))


-- Declarations ----------------------------------------------------------------

-- | Fully qualified declarations.
data Decl = Decl { declName   :: Name
                 , declExport :: Export
                 , declType   :: Schema
                 , declBody   :: Expr
                 } deriving (Show,Data,Typeable)

instance Exported Decl where
  exportSpec = declExport

instance BoundVars Decl where
  boundVars d = Set.singleton (declName d)

instance FreeVars Decl where
  freeVars d = freeVars (declBody d)

-- | Pretty-print a declaration without its export annotation.
ppDecl :: Bool -> Decl -> [PPDoc]
ppDecl withExp d | withExp   = [ hang (ppr (declExport d)) 2 (vcat body) ]
                 | otherwise = body
  where
  body = [ ppr (declName d) <+> char ':' <+> ppr (declType d)
         , ppr (declName d) <+> char '=' <+> ppr (declBody d) ]

-- | Determine if a declaration is monomorphic.
isMono :: Decl -> Bool
isMono d = null (sParams (declType d))


-- Variable Introduction -------------------------------------------------------

-- | Typed variable introduction.
data Match = MSplit Match Match
           | MPat   Pat   Match
           | MGuard Pat   Expr  Type Match
           | MFail Type
             deriving (Show,Data,Typeable)

instance FreeVars Match where
  freeVars m = case m of
    MSplit l r      -> freeVars l `Set.union` freeVars r
    MPat p m'       -> freeVars (p,m')   Set.\\ boundVars p
    MGuard p e _ m' -> freeVars (p,e,m') Set.\\ boundVars p
    MFail _         -> Set.empty

instance Pretty Match where
  ppr m = case m of
    MSplit l r -> pp l $$ pp r

    MPat p m' -> hang (ppPrec 10 p)
                    2 (pp m')

    MGuard p e ty m' -> cat
      [ pp p <+> text "<-" <+> parens (pp e <+> pp ty)
      , char ',' <+> pp m'
      ]

    MFail _ -> empty

-- | The type of a match.
matchType :: Match -> Type
matchType m = case m of
  MSplit l _      -> matchType l
  MPat p m'       -> patType p `tArrow` matchType m'
  MGuard _ _ _ m' -> matchType m'
  MFail ty        -> ty


-- Variable Patterns -----------------------------------------------------------

data Pat = PVar Name Type
         | PCon Name [Name] Type
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
  PCon _ ps _ -> ps
  PWildcard _ -> []

instance BoundVars Pat where
  boundVars pat = case pat of
    PCon _ ps _ -> Set.fromList ps
    PVar n _    -> Set.singleton n
    PWildcard _ -> Set.empty

instance FreeVars Pat where
  freeVars p = case p of
    PCon n _ _  -> Set.singleton n
    PVar{}      -> Set.empty
    PWildcard{} -> Set.empty

instance Pretty Pat where
  ppr pat = parens $ case pat of
    PVar n ty     -> ppr n    <+> text ":"                       <+> ppr ty
    PCon qn ps ty -> ppr qn   <+> hsep (map ppr ps) <+> text ":" <+> ppr ty
    PWildcard ty  -> char '_' <+> text ":"                       <+> ppr ty


-- Expressions -----------------------------------------------------------------

data Expr = EAbs Name Expr
          | EApp Expr [Expr]
          | EType Type
          | ECase Expr Match
          | ELet [Decl] Expr
          | EVar Name
          | ELit Literal
            deriving (Show,Data,Typeable)

appT :: Expr -> [Type] -> Expr
appT f ts = app f (map EType ts)

-- | Apply evidence.
app :: Expr -> [Expr] -> Expr
app f [] = f
app f es = case f of
  EApp f' es' -> EApp f' (es' ++ es)
  _           -> EApp f          es

letIn :: [Decl] -> Expr -> Expr
letIn [] e = e
letIn ds e = ELet ds e

instance FreeVars Expr where
  freeVars tm = case tm of
    EAbs x e  -> Set.delete x (freeVars e)
    EApp f x  -> freeVars (f,x)
    ECase e m -> freeVars (e,m)
    ELet ds t -> freeVars (t,ds) Set.\\ Set.fromList (map declName ds)
    EVar n    -> Set.singleton n
    EType ty  -> freeVars ty
    ELit _    -> Set.empty

instance Pretty Expr where
  ppr tm = case tm of
    EAbs x e  -> hang (text "\\" <+> pp x <+> text "->")
                    2 (pp e)
    EApp f xs -> optParens 10 (ppr f <+> fsep (map (ppPrec 10) xs))

    ECase e m  -> optParens 10 (ppCase e m)
    ELet ds e  -> optParens 10
                $ text "let" <+> layout (concatMap (ppDecl False) ds)
              <+> text "in"  <+> ppr e
    EVar n     -> ppr n
    ELit l     -> ppr l

    EType ty   -> ppr ty

ppCase :: Expr -> Match -> PPDoc
ppCase e m = hang (text "case" <+> ppr e <+> text "of")
                2 (pp m)
