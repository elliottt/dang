{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}

module Dang.Syntax.AST where

import Dang.ModuleSystem.Export ( Export(..) )
import Dang.ModuleSystem.QualName
import Dang.Traversal ( Data, Typeable )
import Dang.Utils.Location
import Dang.Utils.Pretty
import Dang.Variables

import Data.List (partition,nub)
import Data.Monoid (mempty)
import qualified Data.Set as Set


-- Parsed AST ------------------------------------------------------------------

-- | The namespace provided by a module.
modNamespace :: Module -> [Name]
modNamespace  = qualNamespace . locValue . modName

-- | A parsed program.
data Module = Module { modName  :: Located QualName
                     , modOpens :: [Located Open]
                     , modDecls :: Block TopDecl
                     } deriving (Show,Data,Typeable)

data Block a = BSingle a
               -- ^ A single declaration.
             | BExport Export (Block a)
               -- ^ An export annotation on a block of declarations.
             | BRec (Block a)
               -- ^ A recursive block of declarations.
             | BComb (Block a) (Block a)
               -- ^ Non-overlapping combination of declarations.
             | BSeq (Block a) (Block a)
               -- ^ Left-to-right combination of declaraitons.
             | BLocal (Block a) (Block a)
               -- ^ Declarations that are local to a group of other
               -- declarations.
             | BSource SrcLoc (Block a)
               -- ^ Source lcoations attached to a block of declaration.
               deriving (Show,Data,Typeable)

-- | Top-level declarations.
data TopDecl = TDDecl Decl
             | TDData DataDecl
               deriving (Show,Data,Typeable)

-- | A module import.
data Open = Open { openMod     :: QualName
                 , openAs      :: Maybe QualName
                 , openHiding  :: Bool
                 , openSymbols :: [OpenSymbol]
                 } deriving (Show,Data,Typeable)

-- | Symbols that can be imported.
data OpenSymbol = OpenTerm Name
                | OpenType Name [Name]
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
                         , primTypeKind :: Kind
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

data Schema = Forall Type
              deriving (Show,Data,Typeable)

-- | Kinds and types use the same surface syntax.
type Kind = Type

data Type = TApp QualName [Type]
          | TInfix Type QualName Type
          | TCon QualName
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
         | PCon QualName [Pat] -- ^ Constructor patterns
         | PWildcard           -- ^ The wildcard pattern
         | PSource SrcLoc Pat  -- ^ Source location
           deriving (Show,Data,Typeable)

data Term = Abs Match
          | Case Term Match
          | Let [Decl] Term
          | App Term [Term]
          | Local Name
          | Global QualName
          | Lit Literal
          | TLoc (Located Term)
            deriving (Show,Data,Typeable)

data Literal = LInt Integer
               deriving (Show,Data,Typeable)
