{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Syntax.AST where

import Pretty
import QualName
import TypeChecker.Types
import Variables

import Data.Graph (SCC(..))
import Data.Graph.SCC (stronglyConnComp)
import Data.Int (Int64)
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
    decls = map (pp 0) (modPrimTerms m)
         ++ map (pp 0) (modPrimTypes m)
         ++ map (pp 0) (modOpens m)
         ++ map (pp 0) (modTyped m)
         ++ map (pp 0) (modUntyped m)

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
  , typedVars   :: [Var]
  , typedBody   :: Term
  } deriving (Eq,Show,Ord)

instance FreeVars TypedDecl where
  freeVars d = ignoreVars (typedBinds d) (freeVars (typedBody d))

instance Pretty TypedDecl where
  pp _ d = semis [ty,body]
    where
    prefix = ppr (typedExport d) <+> text (typedName d)
    body = prefix <+> hsep (map text (typedVars d)) <+> char '='
       <+> ppr (typedBody d)
    ty   = prefix <+> text "::" <+> ppr (typedType d)
  ppList _ ds = semis (map ppr ds)

-- | The variables that a typed binding introduces to its body.
typedBinds :: TypedDecl -> [Var]
typedBinds d = typedName d : typedVars d

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
  , untypedVars   :: [Var]
  , untypedBody   :: Term
  } deriving (Eq,Show,Ord)

instance FreeVars UntypedDecl where
  freeVars d = ignoreVars (untypedBinds d) (freeVars (untypedBody d))

instance Pretty UntypedDecl where
  pp _ d = ppr (untypedExport d) <+> text (untypedName d)
       <+> hsep (map text (untypedVars d)) <+> char '='
       <+> ppr (untypedBody d)
  ppList _ ds = semis (map ppr ds)

-- | The variables that an untyped declaration introduces to its body.
untypedBinds :: UntypedDecl -> [Var]
untypedBinds d = untypedName d : untypedVars d


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


-- Terms -----------------------------------------------------------------------

type Var = String

data Term
  = Abs [Var] Term
  | Let [TypedDecl] [UntypedDecl] Term
  | App Term [Term]
  | Local Name
  | Global QualName
  | Lit Literal
  | Prim Var
    deriving (Eq,Show,Ord)

instance FreeVars Term where
  freeVars (Abs vs t)    = ignoreVars vs (freeVars t)
  freeVars (Let ts us t) = ignoreVars (letBinds ts us)
                         $ Set.unions [freeVars ts, freeVars us, freeVars t]
  freeVars (App f xs)    = freeVars f `Set.union` freeVars xs
  freeVars (Lit l)       = freeVars l
  freeVars (Local x)     = Set.singleton (simpleName x)
  freeVars (Global _)    = Set.empty
  freeVars (Prim _)      = Set.empty

instance Pretty Term where
  pp p t = case t of
    Abs vs b    -> optParens (p > 0)
                 $ char '\\' <> ppList 0 vs <> text "->" <> ppr b
    Let ts us e -> optParens (p > 0)
                $ text "let" <+> braces (semis (map ppr ts ++ map ppr us)) <+>
                  text "in"  <+> pp 0 e
    App f xs    -> optParens (p > 0) (ppr f <+> ppList 1 xs)
    Local n     -> ppr n
    Global n    -> ppr n
    Lit l       -> ppr l
    Prim n      -> char '#' <> text n

letBinds :: [TypedDecl] -> [UntypedDecl] -> [Var]
letBinds ts us = concatMap typedBinds ts ++ concatMap untypedBinds us

-- | Collapse an abstraction into its arguments, and the body.
splitAbs :: Term -> ([Var],Term)
splitAbs t = loop t id
  where
  loop (Abs as b) f = loop b ((++ as) . f)
  loop b          f = (f [], b)

-- | Collapse an application into its arguments, and the function to be called.
splitApp :: Term -> (Term,[Term])
splitApp (App f xs) = (f,xs)
splitApp t          = (t,[])

lambda :: [Var] -> Term -> Term
lambda [] t = t
lambda as t = Abs as t

apply :: Term -> [Term] -> Term
apply f [] = f
apply f xs = App f xs


-- Literals --------------------------------------------------------------------

data Literal
  = LInt Int64
    deriving (Eq,Show,Ord)

instance FreeVars Literal where
  freeVars _ = Set.empty

instance Pretty Literal where
  pp _ (LInt i) = ppr i


-- Utilities -------------------------------------------------------------------

deriving instance Show a => Show (SCC a)

ignoreVars :: [Var] -> Set.Set QualName -> Set.Set QualName
ignoreVars vs fvs = fvs Set.\\ Set.fromList (map simpleName vs)

