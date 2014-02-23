{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Syntax.AST where

import Dang.ModuleSystem.Export ( Export(..) )
import Dang.ModuleSystem.QualName
import Dang.Traversal ( Data, Typeable )
import Dang.Utils.Location
import Dang.Utils.Pretty
import Dang.Variables

import           Data.List ( intersperse )
import           Data.Monoid ( mempty, mappend )
import qualified Data.Set as Set


-- Parsed AST ------------------------------------------------------------------

-- | A parsed program.
data Module = Module { modName  :: Located ModName
                     , modDecls :: Block TopDecl
                     } deriving (Show,Data,Typeable)

-- | Binding blocks.
data Block a = BSingle a
               -- ^ A single declaration.
             | BExport Export (Block a)
               -- ^ An export annotation on a block of declarations.
             | BRec (Block a)
               -- ^ A recursive block of declarations.
             | BComb (Block a) (Block a)
               -- ^ Non-overlapping combination of declarations.
             | BSeq (Block a) (Block a)
               -- ^ Left-to-right combination of declaraitons.  The resulting
               -- binding group will prefer names from the right, shadowing
               -- names on the left.
             | BLocal (Block a) (Block a)
               -- ^ Declarations that are local to a group of other
               -- declarations.
             | BSource SrcLoc (Block a)
               -- ^ Source lcoations attached to a block of declaration.
             | BEmpty
               -- ^ No declarations.
               deriving (Show,Data,Typeable)

elimBCombs :: Block a -> [Block a]
elimBCombs b = case b of
  BComb l r -> elimBCombs l ++ elimBCombs r
  BEmpty    -> []
  _         -> [b]

elimBSeqs :: Block a -> [Block a]
elimBSeqs b = case b of
  BSeq l r -> elimBSeqs l ++ elimBSeqs r
  BEmpty   -> []
  _        -> [b]

-- | Top-level declarations.
data TopDecl = TDDecl Decl
             | TDData DataDecl
               deriving (Show,Data,Typeable)

-- | Declarations that can show up anywhere.
data Decl = DBind (Located Bind)     -- ^ Name bindings
          | DSig (Located Signature) -- ^ Type signatures
          | DOpen (Located Open)     -- ^ Module imports
            deriving (Show,Data,Typeable)

-- | A module import.
data Open = Open { openMod     :: Located ModName
                 , openAs      :: Maybe (Located ModName)
                 , openHiding  :: Bool
                 , openSymbols :: [Located OpenSymbol]
                 } deriving (Show,Data,Typeable)

-- | Symbols that can be imported.
data OpenSymbol = OpenTerm String
                | OpenType String [String]
                  deriving (Show,Data,Typeable)

-- | Function binding.
data Bind = Bind { bindName :: Name
                 , bindType :: Maybe Schema
                 , bindBody :: Match
                 } deriving (Show,Data,Typeable)

-- | A name with a signature.
data Signature = Signature { sigNames  :: [Located Name]
                           , sigSchema :: Schema
                           } deriving (Show,Data,Typeable)

-- | A primitive term name, with a signature.
data PrimTerm = PrimTerm { primTermName :: Located Name
                         , primTermType :: Schema
                         } deriving (Show,Data,Typeable)

-- | A primitive type, with a kind signature.
data PrimType = PrimType { primTypeName :: Name
                         , primTypeKind :: Schema
                         } deriving (Show,Data,Typeable)

-- | GADT declaration.
data DataDecl = DataDecl { dataName   :: Name
                         , dataArity  :: !Int
                         , dataKind   :: Kind
                         , dataExport :: Export
                         , dataParams :: [Name]
                         , dataGroups :: [ConstrGroup]
                         } deriving (Show,Data,Typeable)

-- | GADT constructor groups.
data ConstrGroup = ConstrGroup { groupArgs    :: [Type]
                               , groupConstrs :: [Constr]
                               } deriving (Show,Data,Typeable)

-- | GADT constructors.
data Constr = Constr { constrName   :: Name
                     , constrFields :: [Type]
                     } deriving (Show,Data,Typeable)

-- | Type schemas, with constraints.
data Schema = Forall [Prop] Type
              deriving (Show,Data,Typeable)

-- | Kinds and types use the same surface syntax.
type Kind = Type

type Prop = Type

data Type = TFun Type Type
          | TApp Type [Type]
          | TTuple [Type]
          | TCon Name
          | TVar Name
          | TSource SrcLoc Type
            deriving (Show,Data,Typeable)

elimTFuns :: Type -> [Type]
elimTFuns ty = case ty of
  TFun l r -> l : elimTFuns r
  _        -> [ty]

data Match = MTerm  Term             -- ^ Body of a match
           | MPat   Pat   Match      -- ^ Pattern matching
           | MGuard Pat   Term Match -- ^ Pattern guards
           | MSplit Match Match      -- ^ Choice
           | MFail                   -- ^ Unconditional failure
           | MSource SrcLoc Match    -- ^ Source locations
             deriving (Show,Data,Typeable)

data Pat = PVar Name           -- ^ Variable introduction
         | PCon Name [Pat]     -- ^ Constructor patterns
         | PWildcard           -- ^ The wildcard pattern
         | PSource SrcLoc Pat  -- ^ Source location
           deriving (Show,Data,Typeable)

data Term = Abs Match
          | Case Term Match
          | Let (Block Decl) Term
          | App Term [Term]
          | Var Name
          | Lit Literal
          | TLoc (Located Term)
            deriving (Show,Data,Typeable)

data Literal = LInt Integer
               deriving (Show,Data,Typeable)


-- Variable Binders-------------------------------------------------------------

instance BoundVars a => BoundVars (Block a) where
  boundVars b = case b of
    BSingle a    -> boundVars a
    BExport _ b' -> boundVars b'
    BRec b'      -> boundVars b'
    BComb l r    -> boundVars [l,r]
    BSeq l r     -> boundVars [l,r]
    BLocal _ b'  -> boundVars b'
    BSource _ b' -> boundVars b'
    BEmpty       -> Set.empty

-- only bindings can bind variables
instance BoundVars Decl where
  boundVars d = case d of
    DBind b -> boundVars b
    DSig _  -> Set.empty
    DOpen _ -> Set.empty

instance BoundVars Bind where
  boundVars b = Set.singleton (bindName b)

instance BoundVars Signature where
  boundVars sig = Set.fromList (map unLoc (sigNames sig))

instance BoundVars Pat where
  boundVars p = case p of
    PVar n       -> Set.singleton n
    PCon qn ps   -> Set.insert qn (boundVars ps)
    PWildcard    -> Set.empty
    PSource _ p' -> boundVars p'


-- Free Variables --------------------------------------------------------------

instance (BoundVars a, FreeVars a) => FreeVars (Block a) where
  freeVars b = case b of
    BSingle a    -> freeVars a
    BExport _ b' -> freeVars b'
    BRec b'      -> freeVars b'
    BComb l r    -> freeVars [l,r]  Set.\\ boundVars [l,r]
    BSeq l r     -> freeVars [l,r]  Set.\\ boundVars l
    BLocal l b'  -> freeVars [l,b'] Set.\\ boundVars l
    BSource _ b' -> freeVars b'
    BEmpty       -> Set.empty

instance FreeVars Decl where
  freeVars d = case d of
    DBind b -> freeVars b
    DSig _  -> Set.empty
    DOpen _ -> Set.empty

instance FreeVars Bind where
  freeVars b = freeVars (bindBody b) Set.\\ boundVars b

instance FreeVars Match where
  freeVars m = case m of
    MTerm tm       -> freeVars tm
    MPat p m'      -> freeVars m'      Set.\\ boundVars p
    MGuard p tm m' -> freeVars (tm,m') Set.\\ boundVars p
    MSplit l r     -> freeVars [l,r]
    MFail          -> Set.empty
    MSource _ m'   -> freeVars m'

instance FreeVars Term where
  freeVars tm = case tm of
    Abs m     -> freeVars m
    Case e m  -> freeVars e `Set.union` freeVars m
    Let b e   -> freeVars (b,e) Set.\\ boundVars b
    App f xs  -> freeVars f `Set.union` freeVars xs
    Var n     -> Set.singleton n
    Lit _     -> Set.empty
    TLoc ltm  -> freeVars ltm


-- Pretty Printing -------------------------------------------------------------

instance Pretty Module where
  ppr m = text "module" <+> ppModName (unLoc (modName m)) <+> text "where"
       $$ pp (modDecls m)

instance Pretty a => Pretty (Block a) where
  ppr b = case b of

    BSingle a -> ppr a

    BComb{} -> layout (map pp (elimBCombs b))

    BExport e b' -> hang (pp e) 2 (pp b')

    BRec b' -> hang (text "rec") 2 (pp b')

    BSeq{} -> layout (map pp (elimBSeqs b))

    BLocal as bs -> hang (text "local") 2 (pp as)
                 $$ hang (text "in") 2 (pp bs)

    BEmpty -> empty

    BSource _ b' -> ppr b'


instance Pretty TopDecl where
  ppr td = case td of
    TDDecl d -> ppr d
    TDData d -> ppr d

instance Pretty Decl where
  ppr d = case d of
    DBind b -> ppr b
    DSig s  -> ppr s
    DOpen o -> ppr o

instance Pretty DataDecl where
  ppr d = empty

instance Pretty Bind where
  ppr b = empty

instance Pretty Signature where
    ppr sig = hang (commas (map pp (sigNames sig)) <+> text ":")
                 2 (pp (sigSchema sig))

instance Pretty Open where
  ppr o = hang (text "open" <+> ppModName (unLoc (openMod o)) <+> altName)
             5 spec
    where
    altName = case openAs o of
      Nothing -> empty
      Just ln -> text "as" <+> ppModName (unLoc ln)

    spec | openHiding o = text "hiding" <+> symbols
         | otherwise    = symbols

    symbols | null (openSymbols o) = empty
            | otherwise            = parens (commas (map pp (openSymbols o)))

instance Pretty OpenSymbol where
  ppr os = case os of
    OpenTerm n      -> text n
    OpenType n cons -> text n <> parens (commas (map text cons))

instance Pretty Schema where
  ppr (Forall ps ty)
    | null ps   = pp ty
    | otherwise = sep [ props, pp ty ]
      where
      props = case ps of
        []     -> empty
        [prop] -> pp prop <+> text "=>"
        _      -> parens (commas (map pp ps)) <+> text "=>"

instance Pretty Type where
  ppr ty = case ty of
    TFun{}        -> optParens 10 $ fsep
                                  $ intersperse (text "->")
                                  $ map (ppPrec 10) (elimTFuns ty)
    TApp f xs     -> optParens 10 (fsep (pp f : map (ppPrec 10) xs))
    TTuple ts     -> parens (commas (map pp ts))
    TCon n        -> pp n
    TVar n        -> pp n
    TSource _ ty' -> ppr ty'

instance Pretty Literal where
  ppr lit = case lit of
    LInt i -> integer i


-- Location Information --------------------------------------------------------

instance HasLocation a => HasLocation (Block a) where
  getLoc b = case b of
    BSource src _ -> src
    _             -> mempty

instance HasLocation TopDecl where
  getLoc td = case td of
    TDDecl d -> getLoc d
    TDData d -> getLoc d

instance HasLocation Decl where
  getLoc d = case d of
    DBind l -> getLoc l
    DSig l  -> getLoc l
    DOpen l -> getLoc l

instance HasLocation DataDecl where
  getLoc d = mempty

instance HasLocation Schema where
  getLoc (Forall ps ty) = getLoc ps `mappend` getLoc ty

instance HasLocation Type where
  getLoc ty = case ty of
    TSource src _ -> src
    _             -> mempty
