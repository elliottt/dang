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


ignoreVars :: [Var] -> Set.Set QualName -> Set.Set QualName
ignoreVars vs fvs = fvs Set.\\ Set.fromList (map simpleName vs)


data Module = Module
  { modName      :: QualName
  , modOpens     :: [Open]
  , modDecls     :: [Decl]
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
         ++ map (pp 0) (modDecls m)

instance Names Module where
  identifiers m = identifiers (modDecls m)

modNamespace :: Module -> [Name]
modNamespace  = qualNamespace . modName


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


type Var = String

data Decl = Decl
  { declExport :: Export
  , declType   :: Maybe (Forall Type)
  , declName   :: Name
  , declVars   :: [Var]
  , declBody   :: Term
  } deriving (Eq,Show,Ord)

instance FreeVars Decl where
  freeVars d = ignoreVars (declBinds d) (freeVars (declBody d))

instance Names Decl where
  identifiers d = identifiers (declBody d)

instance Pretty Decl where
  pp _ d = semis [ty,body]
    where
    prefix = pp 0 (declExport d) <+> text (declName d)
    body = prefix <+> hsep (map text (declVars d)) <+> char '='
       <+> pp 0 (declBody d)
    ty   = maybe empty (\t -> prefix <+> text "::" <+> pp 0 t) (declType d)
  ppList _ ds = semis (map (pp 0) ds)

declNames :: [Decl] -> [Var]
declNames  = map declName

hasArguments :: Decl -> Bool
hasArguments  = not . null . declVars

declBinds :: Decl -> [Var]
declBinds d = declName d : declVars d


deriving instance Show a => Show (SCC a)

sccModule :: Module -> [SCC Decl]
sccModule m = sccDecls (modNamespace m) (modDecls m)

sccDecls :: [Name] -> [Decl] -> [SCC Decl]
sccDecls ns = stronglyConnComp . declsFvGraph ns

declsFvGraph :: [Name] -> [Decl] -> [(Decl,QualName,[QualName])]
declsFvGraph ns ds = graph
  where
  graph = [ (d, qualName ns (declName d), Set.toList (freeVars d)) | d <- ds ]


data Term
  = Abs [Var] Term
  | Let [Decl] Term
  | App Term [Term]
  | Local Name
  | Global QualName
  | Lit Literal
  | Prim Var
    deriving (Eq,Show,Ord)

instance FreeVars Term where
  freeVars (Abs vs t) = ignoreVars vs (freeVars t)
  freeVars (Let ds t) = ignoreVars (concatMap declBinds ds)
                      $ freeVars ds `Set.union` freeVars t
  freeVars (App f xs) = freeVars f `Set.union` freeVars xs
  freeVars (Lit l)    = freeVars l
  freeVars (Local x)  = Set.singleton (simpleName x)
  freeVars (Global _) = Set.empty
  freeVars (Prim _)   = Set.empty

instance Names Term where
  identifiers (Abs _  t) = identifiers t
  identifiers (Let ds t) = identifiers ds `Set.union` identifiers t
  identifiers (App f xs) = identifiers f `Set.union` identifiers xs
  identifiers (Local n)  = Set.singleton (simpleName n)
  identifiers (Global n) = Set.singleton n
  identifiers (Lit _)    = Set.empty
  identifiers (Prim n)   = Set.singleton (primName n)

instance Pretty Term where
  pp p t =
    case t of
      Abs vs b -> optParens (p > 0)
                $ char '\\' <> ppList 0 vs <> text "->" <> pp 0 b
      Let ds e -> optParens (p > 0)
                $ text "let" <+> braces (semis (map (pp 0) ds)) <+>
                  text "in"  <+> pp 0 e
      App f xs -> optParens (p > 0) (pp 0 f <+> ppList 1 xs)
      Local n  -> pp 0 n
      Global n -> pp 0 n
      Lit l    -> pp 0 l
      Prim n   -> char '#' <> text n

instance Num Term where
  fromInteger i = Lit (LInt (fromIntegral i))
  a + b         = App (Prim "prim_add_i")    [a,b]
  a * b         = App (Prim "prim_mul_i")    [a,b]
  a - b         = App (Prim "prim_sub_i")    [a,b]
  abs x         = App (Prim "prim_abs_i")    [x]
  signum x      = App (Prim "prim_signum_i") [x]

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

data Literal
  = LInt Int64
    deriving (Eq,Show,Ord)

instance FreeVars Literal where
  freeVars _ = Set.empty

instance Pretty Literal where
  pp _ (LInt i) = ppr i


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
