{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Syntax.AST where

import Pretty
import QualName
import TypeChecker.Types
import Variables

import Data.Graph (SCC(..))
import Data.Graph.SCC (stronglyConnComp)
import qualified Data.Set as Set


-- Modules ---------------------------------------------------------------------

data Module = Module
  { modName      :: QualName
  , modOpens     :: [Open]
  , modTyped     :: [TypedDecl]
  , modUntyped   :: [UntypedDecl]
  , modPrimTerms :: [PrimTerm]
  , modPrimTypes :: [PrimType]
  } deriving (Show)

instance Pretty Module where
  pp _ m = text "module" <+> pp 0 (modName m) <+> text "where"
       $+$ declBlock decls
    where
    decls = map       ppr         (modPrimTypes m)
         ++ map       ppr         (modPrimTerms m)
         ++ map       ppr         (modOpens m)
         ++ concatMap ppTypedDecl (modTyped m)
         ++ map       ppr         (modUntyped m)

modNamespace :: Module -> [Name]
modNamespace  = qualNamespace . modName


-- Open Declarations -----------------------------------------------------------

data Open = Open
  { openMod     :: QualName
  , openAs      :: (Maybe QualName)
  , openHiding  :: Bool
  , openSymbols :: [Name]
  } deriving (Eq,Ord,Show)

instance Pretty Open where
  pp _ o = text "open" <+> pp 0 (openMod o) <+> name <+> hiding
    where
    name = maybe empty (pp 0) (openAs o)
    hiding | openHiding o && null (openSymbols o) = empty
           | openHiding o                         = text "hiding" <+> symList
           | otherwise                            = symList
    symList = parens (commas (map (pp 0) (openSymbols o)))


data Export = Public | Private
    deriving (Eq,Show,Ord)

instance Pretty Export where
  pp _ Public  = text "public"
  pp _ Private = text "private"


-- Typed Declarations ----------------------------------------------------------

data TypedDecl = TypedDecl
  { typedExport :: Export
  , typedType   :: Forall Type
  , typedName   :: Name
  , typedBody   :: Match
  } deriving (Eq,Show,Ord)

instance FreeVars TypedDecl where
  freeVars = freeVars . typedBody

ppTypedDecl :: TypedDecl -> [Doc]
ppTypedDecl d = [sig,body]
  where
  sig    = prefix <+> text "::" <+> ppr (typedType d)
  prefix = ppr (typedExport d) <+> text (typedName d)
  (as,b) = ppMatch (typedBody d)
  body   = prefix <+> hsep as <+> char '=' <+> b

-- | Create a typed declaration from an untyped one.
mkTypedDecl :: UntypedDecl -> Forall Type -> TypedDecl
mkTypedDecl u ty = TypedDecl
  { typedExport = untypedExport u
  , typedType   = ty
  , typedName   = untypedName u
  , typedBody   = untypedBody u
  }

sccTypedDecls :: Namespace -> [TypedDecl] -> [SCC TypedDecl]
sccTypedDecls ns = stronglyConnComp . typedDeclsFvGraph ns

typedDeclsFvGraph :: Namespace -> [TypedDecl]
                  -> [(TypedDecl,QualName,[QualName])]
typedDeclsFvGraph ns ds = graph
  where
  graph = [ (d, qualName ns (typedName d), Set.toList (freeVars d)) | d <- ds ]


-- Untyped Declarations --------------------------------------------------------

data UntypedDecl = UntypedDecl
  { untypedExport :: Export
  , untypedName   :: Name
  , untypedBody   :: Match
  } deriving (Eq,Show,Ord)

instance FreeVars UntypedDecl where
  freeVars = freeVars . untypedBody

instance Pretty UntypedDecl where
  pp _ d = ppr (untypedExport d) <+> text (untypedName d)
       <+> hsep as <+> char '=' <+> b
    where
    (as,b) = ppMatch (untypedBody d)
  ppList _ ds = semis (map ppr ds)


-- Primitive Term Declarations -------------------------------------------------

data PrimTerm = PrimTerm
  { primTermName :: String
  , primTermType :: Forall Type
  } deriving (Eq,Show,Ord)

instance Pretty PrimTerm where
  pp _ p = text "primitive" <+> text (primTermName p)
       <+> text "::" <+> pp 0 (primTermType p)


-- Primitive Type Declarations -------------------------------------------------

data PrimType = PrimType
  { primTypeName :: String
  , primTypeKind :: Kind
  } deriving (Eq,Show,Ord)

instance Pretty PrimType where
  pp _ p = text "primitive type" <+> text (primTypeName p)
       <+> text "::" <+> pp 0 (primTypeKind p)


-- Variable Introduction -------------------------------------------------------

data Match
  = MTerm Term      -- ^ Body of a match
  | MPat  Pat Match -- ^ Pattern matching
    deriving (Eq,Show,Ord)

instance FreeVars Match where
  freeVars (MTerm tm) = freeVars tm
  freeVars (MPat p m) = freeVars m Set.\\ freeVars p

instance Pretty Match where
  pp _ (MTerm tm) = ppr tm
  pp _ (MPat p m) = pp 1 p <+> text "->" <+> ppr m

-- | Pretty printing of a @Match@, in the context of a declaration.
matchDecl :: Match -> Doc
matchDecl m = case ppMatch m of
  ([],b) -> b
  (as,b) -> hsep as <+> text "=" <+> b

-- | Pretty print the arguments, and body of a @Match@.
ppMatch :: Match -> ([Doc],Doc)
ppMatch (MTerm tm) = ([], ppr tm)
ppMatch (MPat p m) = (pp 1 p:as, b)
  where
  (as,b) = ppMatch m


-- Pattern Matching ------------------------------------------------------------

data Pat
  = PVar Name -- ^ Variable introduction
  | PWildcard -- ^ The wildcard pattern
    deriving (Eq,Show,Ord)

instance FreeVars Pat where
  freeVars (PVar n)   = Set.singleton (simpleName n)
  freeVars  PWildcard = Set.empty

instance Pretty Pat where
  pp _ (PVar n)   = text n
  pp _  PWildcard = char '_'

-- | Variables introduced by a pattern.
patVars :: Pat -> [Var]
patVars (PVar n)  = [n]
patVars PWildcard = []


-- Terms -----------------------------------------------------------------------

type Var = String

data Term
  = Abs Match
  | Let [TypedDecl] [UntypedDecl] Term
  | App Term [Term]
  | Local Name
  | Global QualName
  | Lit Literal
  | Prim Var
    deriving (Eq,Show,Ord)

instance FreeVars Term where
  freeVars (Abs m)       = freeVars m
  freeVars (Let ts us t) = ignoreVars (letBinds ts us)
                         $ Set.unions [freeVars ts, freeVars us, freeVars t]
  freeVars (App f xs)    = freeVars f `Set.union` freeVars xs
  freeVars (Lit l)       = freeVars l
  freeVars (Local x)     = Set.singleton (simpleName x)
  freeVars (Global _)    = Set.empty
  freeVars (Prim _)      = Set.empty

instance Pretty Term where
  pp p t = case t of
    Abs m       -> optParens (p > 0) (ppAbs m)
    Let ts us e -> optParens (p > 0) (ppLet ts us e)
    App f xs    -> optParens (p > 0) (ppr f <+> ppList 1 xs)
    Local n     -> ppr n
    Global n    -> ppr n
    Lit l       -> ppr l
    Prim n      -> char '#' <> text n

ppLet :: [TypedDecl] -> [UntypedDecl] -> Term -> Doc
ppLet ts us e = text "let" <+> declBlock decls <+> text "in" <+> ppr e
  where
  decls = concatMap ppTypedDecl ts ++ map ppr us

ppAbs :: Match -> Doc
ppAbs m = case ppMatch m of
  (as,b) -> char '\\' <> hsep as <+> text "->" <+> b

letBinds :: [TypedDecl] -> [UntypedDecl] -> [Var]
letBinds ts us = map typedName ts ++ map untypedName us

apply :: Term -> [Term] -> Term
apply f [] = f
apply f xs = App f xs


-- Literals --------------------------------------------------------------------

data Literal
  = LInt Integer
    deriving (Eq,Show,Ord)

instance FreeVars Literal where
  freeVars _ = Set.empty

instance Pretty Literal where
  pp _ (LInt i) = ppr i


-- Utilities -------------------------------------------------------------------

deriving instance Show a => Show (SCC a)

ignoreVars :: [Var] -> Set.Set QualName -> Set.Set QualName
ignoreVars vs fvs = fvs Set.\\ Set.fromList (map simpleName vs)

