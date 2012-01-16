{-# LANGUAGE StandaloneDeriving #-}

module Syntax.AST where

import ModuleSystem.Export
    (Exported(..),Export(..),isExported,ppPublic,ppPrivate,groupByExport)
import Pretty
import QualName
import TypeChecker.Types
import Variables

import Data.List (partition)
import qualified Data.Set as Set


-- Modules ---------------------------------------------------------------------

data Module = Module
  { modName      :: QualName
  , modOpens     :: [Open]
  , modTyped     :: [TypedDecl]
  , modUntyped   :: [UntypedDecl]
  , modPrimTerms :: [PrimTerm]
  , modPrimTypes :: [PrimType]
  , modDatas     :: [DataDecl]
  } deriving (Show)

instance Pretty Module where
  pp _ m = text "module" <+> pp 0 (modName m) <+> text "where"
        $$ vcat decls
    where
    decls = map       ppr         (modOpens m)
         ++ map       ppr         (modPrimTypes m)
         ++ map       ppr         (modPrimTerms m)
         ++ map       ppr         (modDatas m)
         ++ [ ppPublic  (vcat public)
            , ppPrivate (vcat private)
            ]
    (uus,rus) = partition isExported (modUntyped m)
    (uts,rts) = partition isExported (modTyped m)

    public = map ppTypedDecl uts
          ++ map ppr         uus

    private = map ppTypedDecl rts
           ++ map ppr         rus

modNamespace :: Module -> [Name]
modNamespace  = qualNamespace . modName


-- Open Declarations -----------------------------------------------------------

data Open = Open
  { openMod     :: QualName
  , openAs      :: (Maybe QualName)
  , openHiding  :: Bool
  , openSymbols :: [OpenSymbol]
  } deriving (Eq,Ord,Show)

instance Pretty Open where
  pp _ o = text "open" <+> ppr (openMod o) <+> name <+> hiding
    where
    name = maybe empty ppr (openAs o)
    hiding | openHiding o && null (openSymbols o) = empty
           | openHiding o                         = text "hiding" <+> symList
           | otherwise                            = symList
    symList = parens (commas (map ppr (openSymbols o)))


-- Symbol Imports --------------------------------------------------------------

data OpenSymbol
  = OpenTerm Name
  | OpenType Name [Name]
    deriving (Eq,Ord,Show)

instance Pretty OpenSymbol where
  pp _ os = case os of
    OpenTerm n    -> ppr n
    OpenType n [] -> ppr n
    OpenType n ns -> ppr n <> parens (commas (map ppr ns))


-- Typed Declarations ----------------------------------------------------------

data TypedDecl = TypedDecl
  { typedExport :: Export
  , typedType   :: Forall Type
  , typedName   :: Name
  , typedBody   :: Match
  } deriving (Eq,Show,Ord)

instance FreeVars TypedDecl where
  freeVars = freeVars . typedBody

instance DefinesName TypedDecl where
  definedName = typedName

instance Exported TypedDecl where
  exportSpec = typedExport

ppTypedDecl :: TypedDecl -> Doc
ppTypedDecl  = ppTypedDecl' empty

ppTypedDecl' :: Doc -> TypedDecl -> Doc
ppTypedDecl' export d = export <+> nest 0 (vcat [sig,body])
  where
  sig    = text (typedName d) <+> text "::" <+> ppr (typedType d)
  body   = text (typedName d) <+> hsep as <+> char '=' <+> b
  (as,b) = ppMatch (typedBody d)

-- | Create a typed declaration from an untyped one.
mkTypedDecl :: UntypedDecl -> Forall Type -> TypedDecl
mkTypedDecl u ty = TypedDecl
  { typedExport = untypedExport u
  , typedType   = ty
  , typedName   = untypedName u
  , typedBody   = untypedBody u
  }


-- Untyped Declarations --------------------------------------------------------

data UntypedDecl = UntypedDecl
  { untypedExport :: Export
  , untypedName   :: Name
  , untypedBody   :: Match
  } deriving (Eq,Show,Ord)

instance FreeVars UntypedDecl where
  freeVars = freeVars . untypedBody

instance Pretty UntypedDecl where
  pp _ d = text (untypedName d) <+> hsep as <+> char '=' <+> b
    where
    (as,b) = ppMatch (untypedBody d)
  ppList _ ds = semis (map ppr ds)

instance DefinesName UntypedDecl where
  definedName = untypedName

instance Exported UntypedDecl where
  exportSpec = untypedExport


-- Primitive Term Declarations -------------------------------------------------

data PrimTerm = PrimTerm
  { primTermName :: Name
  , primTermType :: Forall Type
  } deriving (Eq,Show,Ord)

instance Pretty PrimTerm where
  pp _ p = text "primitive" <+> text (primTermName p)
       <+> text "::" <+> pp 0 (primTermType p)

instance FreeVars PrimTerm where
  freeVars = freeVars . primTermType

instance DefinesName PrimTerm where
  definedName = primTermName


-- Primitive Type Declarations -------------------------------------------------

data PrimType = PrimType
  { primTypeName :: Name
  , primTypeKind :: Kind
  } deriving (Eq,Show,Ord)

instance Pretty PrimType where
  pp _ p = text "primitive type" <+> text (primTypeName p)
       <+> text "::" <+> pp 0 (primTypeKind p)


-- Data Declarations -----------------------------------------------------------

data DataDecl = DataDecl
  { dataName   :: Name
  , dataArity  :: !Int
  , dataKind   :: Kind
  , dataExport :: Export
  , dataGroups :: [Forall ConstrGroup]
  } deriving (Show)

instance Pretty DataDecl where
  pp _ d = text "data"
       <+> nest 0 (vcat (map step (dataGroups d)))
    where
    step = ppConstrGroup (dataExport d) (simpleName (dataName d)). forallData

instance FreeVars DataDecl where
  freeVars d = Set.delete (simpleName (dataName d)) (freeVars (dataGroups d))

instance DefinesName DataDecl where
  definedName = dataName

instance Exported DataDecl where
  exportSpec = dataExport


-- Constructor Groups ----------------------------------------------------------

data ConstrGroup = ConstrGroup
  { groupArgs    :: [Type]
  , groupConstrs :: [Constr]
  } deriving (Show)

ppConstrGroup :: Export -> QualName -> ConstrGroup -> Doc
ppConstrGroup x qn g = ppr ty <+> char '='
                    $$ nest 2 (ppConstrBlock (Just x) (groupConstrs g))
  where
  ty = foldl TApp (TCon qn) (groupArgs g)

instance FreeVars ConstrGroup where
  freeVars g = freeVars (groupArgs g) `Set.union` freeVars (groupConstrs g)


-- Data Constructors -----------------------------------------------------------

data Constr = Constr
  { constrName   :: Name
  , constrExport :: Export
  , constrFields :: [Type]
  } deriving (Show)

ppConstrBlock :: Maybe Export -> [Constr] -> Doc
ppConstrBlock mb = vcat . map step . groupByExport
  where

  hideExport = case mb of
    Nothing -> const False
    Just x  -> \ c -> exportSpec c == x

  step []       = empty
  step cs@(c:_) = export (vcat (map ppr cs))
    where
    export | hideExport c = id
           | isExported c = ppPublic
           | otherwise    = ppPrivate

instance Pretty Constr where
  pp _ c = ppr (constrName c) <+> hsep (map ppr (constrFields c))

instance FreeVars Constr where
  freeVars c =
    Set.delete (simpleName (constrName c)) (freeVars (constrFields c))

instance DefinesName Constr where
  definedName = constrName

instance Exported Constr where
  exportSpec = constrExport

test = DataDecl
  { dataName   = "Foo"
  , dataArity  = 1
  , dataKind   = kstar `karrow` kstar
  , dataExport = Private
  , dataGroups =
    [ Forall [] ConstrGroup
      { groupArgs    = [TCon (simpleName "Int")]
      , groupConstrs =
        [ Constr
          { constrName   = "Just"
          , constrExport = Private
          , constrFields = [TCon (simpleName "Int")]
          }
        ]
      }
    , Forall [a] ConstrGroup
      { groupArgs    = [TVar (GVar a)]
      , groupConstrs =
        [ Constr
          { constrName   = "Nothing"
          , constrExport = Public
          , constrFields = [TVar (GVar a)]
          }
        , Constr
          { constrName   = "Something"
          , constrExport = Private
          , constrFields = []
          }
        , Constr
          { constrName   = "Other"
          , constrExport = Public
          , constrFields = []
          }
        ]
      }
    ]
  }

  where
  a = TParam
    { paramIndex      = 0
    , paramFromSource = True
    , paramName       = "a"
    , paramKind       = kstar
    }


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
    deriving (Eq,Show,Ord)

instance FreeVars Term where
  freeVars (Abs m)       = freeVars m
  freeVars (Let ts us t) = ignoreVars (letBinds ts us)
                         $ Set.unions [freeVars ts, freeVars us, freeVars t]
  freeVars (App f xs)    = freeVars f `Set.union` freeVars xs
  freeVars (Lit l)       = freeVars l
  freeVars (Local x)     = Set.singleton (simpleName x)
  freeVars (Global qn)   = Set.singleton qn

instance Pretty Term where
  pp p t = case t of
    Abs m       -> optParens (p > 0) (ppAbs m)
    Let ts us e -> optParens (p > 0) (ppLet ts us e)
    App f xs    -> optParens (p > 0) (ppr f <+> ppList 1 xs)
    Local n     -> ppr n
    Global n    -> ppr n
    Lit l       -> ppr l

ppLet :: [TypedDecl] -> [UntypedDecl] -> Term -> Doc
ppLet ts us e = text "let" <+> nest 0 (vcat decls) $$ text " in" <+> ppr e
  where
  decls = map ppTypedDecl ts ++ map ppr us

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

ignoreVars :: [Var] -> Set.Set QualName -> Set.Set QualName
ignoreVars vs fvs = fvs Set.\\ Set.fromList (map simpleName vs)

