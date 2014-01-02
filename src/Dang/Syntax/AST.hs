{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Syntax.AST where

import Dang.ModuleSystem.Export ( Export(..) )
import Dang.ModuleSystem.QualName
import Dang.Traversal ( Data, Typeable )
import Dang.Utils.Location
import Dang.Utils.Pretty
import Dang.Variables

import qualified Data.Set as Set


-- Parsed AST ------------------------------------------------------------------

-- | A parsed program.
data Module = Module { modName  :: Located ModName
                     , modOpens :: [Located Open]
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

-- | Top-level declarations.
data TopDecl = TDDecl Decl
             | TDData DataDecl
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

data Decl = DBind Bind
          | DSig Signature
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

data Type = TApp Type [Type]
          | TInfix Type Name Type
          | TCon Name
          | TVar Name
            deriving (Show,Data,Typeable)

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

instance BoundVars Decl where
  boundVars d = case d of
    DBind b -> boundVars b
    DSig s  -> boundVars s

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

instance Pretty Literal where
  ppr lit = case lit of
    LInt i -> integer i
