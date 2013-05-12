{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Syntax.AST where

import Dang.ModuleSystem.Export
    (Exported(..),Export(..),isExported,ppPublic,ppPrivate,groupByExport)
import Dang.Pretty
import Dang.ModuleSystem.QualName
import Dang.Traversal (Data,Typeable)
import Dang.TypeChecker.Types
import Dang.Variables

import Data.List (partition,nub)
import Language.Haskell.TH.Syntax (liftString,Lift(..))
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
  } deriving (Show,Data,Typeable)

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
  } deriving (Eq,Ord,Show,Data,Typeable)

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
    deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty OpenSymbol where
  pp _ os = case os of
    OpenTerm n    -> ppr n
    OpenType n [] -> ppr n
    OpenType n ns -> ppr n <> parens (commas (map ppr ns))


-- Typed Declarations ----------------------------------------------------------

data TypedDecl = TypedDecl
  { typedExport :: Export
  , typedType   :: Scheme
  , typedName   :: Name
  , typedBody   :: Match
  } deriving (Eq,Show,Ord,Data,Typeable)

instance Lift TypedDecl where
  lift td = [| TypedDecl
    { typedName   = $(liftString (typedName td))
    , typedType   = $(lift       (typedType td))
    , typedExport = $(lift       (typedExport td))
    , typedBody   = $(lift       (typedBody td))
    } |]

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
mkTypedDecl :: UntypedDecl -> Scheme -> TypedDecl
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
  } deriving (Eq,Show,Ord,Data,Typeable)

instance Lift UntypedDecl where
  lift ud = [| UntypedDecl
    { untypedExport = untypedExport ud
    , untypedName   = untypedName   ud
    , untypedBody   = untypedBody   ud
    } |]

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
  , primTermType :: Scheme
  } deriving (Eq,Show,Ord,Data,Typeable)

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
  } deriving (Eq,Show,Ord,Data,Typeable)

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
  } deriving (Show,Data,Typeable)

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
  } deriving (Show,Data,Typeable)

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
  } deriving (Show,Data,Typeable)

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
  pp _ c = ppr (constrName c) <+> hsep (map (pp 2) (constrFields c))

instance FreeVars Constr where
  freeVars c =
    Set.delete (simpleName (constrName c)) (freeVars (constrFields c))

instance DefinesName Constr where
  definedName = constrName

instance Exported Constr where
  exportSpec = constrExport


-- Variable Introduction -------------------------------------------------------

data Match
  = MTerm  Term             -- ^ Body of a match
  | MPat   Pat   Match      -- ^ Pattern matching
  | MGuard Pat   Term Match -- ^ Pattern guards
  | MSplit Match Match      -- ^ Choice
  | MFail                   -- ^ Unconditional failure
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift Match where
  lift m = case m of
    MTerm tm      -> [| MTerm  $(lift tm)                      |]
    MPat p m'     -> [| MPat   $(lift p) $(lift m')            |]
    MGuard p n m' -> [| MGuard $(lift p) $(lift n)  $(lift m') |]
    MSplit l r    -> [| MSplit $(lift l) $(lift r)             |]
    MFail         -> [| MFail                                  |]

instance FreeVars Match where
  freeVars m = case m of
    MTerm tm      -> freeVars tm
    MPat p m'     -> freeVars m' Set.\\ freeVars p
    MGuard p e m' -> (freeVars e `Set.union` freeVars m') Set.\\ freeVars p
    MSplit l r    -> freeVars l `Set.union` freeVars r
    MFail         -> Set.empty

instance Pretty Match where
  pp _ m = case m of
    MTerm tm      -> ppr tm
    MPat a m'     -> ppMatchCon (pp 1 a) m'
    MGuard a n m' -> ppMatchCon (ppr a <+> text "<-" <+> ppr n) m'
    MSplit l r    -> ppr l $$ ppr r
    MFail         -> empty

-- | Pretty-print the connective for two matches.
ppMatchCon :: Doc -> Match -> Doc
ppMatchCon l m = case m of
  MTerm tm  -> sep [ l <+> text "->", ppr tm ]
  _         -> sep [ l <>  char ',' , ppr m  ]

-- | Pretty printing of a @Match@, in the context of a declaration.
matchDecl :: Match -> Doc
matchDecl m = case ppMatch m of
  ([],b) -> b
  (as,b) -> hsep as <+> text "=" <+> b

-- | Pretty print the arguments, and body of a @Match@.
ppMatch :: Match -> ([Doc],Doc)
ppMatch m = case m of
  MPat p m' ->
    let (as,b) = ppMatch m'
     in (pp 1 p:as, b)
  _         -> ([], ppr m)


-- Pattern Matching ------------------------------------------------------------

data Pat
  = PVar Name           -- ^ Variable introduction
  | PCon QualName [Pat] -- ^ Constructor patterns
  | PWildcard           -- ^ The wildcard pattern
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift Pat where
  lift p = case p of
    PVar n     -> [| PVar $(liftString n)            |]
    PCon qn ps -> [| PCon $(lift qn)      $(lift ps) |]
    PWildcard  -> [| PWildcard                       |]

instance FreeVars Pat where
  freeVars p = case p of
    PVar n     -> Set.singleton (simpleName n)
    PCon qn ps -> Set.insert qn (freeVars ps)
    PWildcard  -> Set.empty

instance Pretty Pat where
  pp p a = case a of
    PVar n     -> text n
    PCon qn ps -> optParens (p > 0 && not (null ps))
                $ ppr qn <+> hsep (map ppr ps)
    PWildcard  -> char '_'

-- | Variables introduced by a pattern.
patVars :: Pat -> [Name]
patVars p = case p of
  PCon _ ps -> nub (concatMap patVars ps)
  PVar n    -> [n]
  PWildcard -> []


-- Terms -----------------------------------------------------------------------

data Term
  = Abs Match
  | Case Term Match
  | Let [TypedDecl] [UntypedDecl] Term
  | App Term [Term]
  | Local Name
  | Global QualName
  | Lit Literal
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift Term where
  lift tm = case tm of
    Abs m       -> [| Abs    $(lift m)                       |]
    Case e m    -> [| Case   $(lift e)  $(lift m)            |]
    Let ts us e -> [| Let    $(lift ts) $(lift us) $(lift e) |]
    App f xs    -> [| App    $(lift f)  $(lift xs)           |]
    Local n     -> [| Local  $(lift n)                       |]
    Global qn   -> [| Global $(lift qn)                      |]
    Lit l       -> [| Lit    $(lift l)                       |]

instance FreeVars Term where
  freeVars tm = case tm of
    Abs m       -> freeVars m
    Case e m    -> freeVars e `Set.union` freeVars m
    Let ts us t -> ignoreVars (letBinds ts us)
                 $ Set.unions [freeVars ts, freeVars us, freeVars t]
    App f xs    -> freeVars f `Set.union` freeVars xs
    Lit l       -> freeVars l
    Local x     -> Set.singleton (simpleName x)
    Global qn   -> Set.singleton qn

instance Pretty Term where
  pp p t = case t of
    Abs m       -> optParens (p > 0) (ppAbs m)
    Case e m    -> optParens (p > 0) (ppCase e m)
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

ppCase :: Term -> Match -> Doc
ppCase e m = text "case" <+> ppr e <+> text "of"
          $$ nest 2 (ppr m)

letBinds :: [TypedDecl] -> [UntypedDecl] -> [Name]
letBinds ts us = map typedName ts ++ map untypedName us

apply :: Term -> [Term] -> Term
apply f [] = f
apply f xs = App f xs


-- Literals --------------------------------------------------------------------

data Literal
  = LInt Integer
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift Literal where
  lift lit = case lit of
    LInt i -> [| LInt $(lift i) |]

instance FreeVars Literal where
  freeVars _ = Set.empty

instance Pretty Literal where
  pp _ (LInt i) = ppr i


-- Utilities -------------------------------------------------------------------

ignoreVars :: [Name] -> Set.Set QualName -> Set.Set QualName
ignoreVars vs fvs = fvs Set.\\ Set.fromList (map simpleName vs)

