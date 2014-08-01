{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.AST where

import Dang.Syntax.Lexeme (Keyword(..))
import Dang.ModuleSystem.QualName
import Dang.ModuleSystem.Types
import Dang.Utils.Location
import Dang.Utils.Pretty
import Dang.Variables

import           Control.Applicative ( pure, (<$>) )
import           Control.Lens ( ignored )
import           Data.Data ( Data )
import           Data.Foldable ( Foldable )
import           Data.List ( intersperse )
import           Data.Monoid ( mempty, mappend )
import qualified Data.Set as Set
import           Data.Traversable ( Traversable )
import           Data.Typeable ( Typeable )
import           GHC.Generics ( Generic )


-- Parsed AST ------------------------------------------------------------------

-- | A parsed program.
data Module = Module { modName  :: Located ModName
                     , modDecls :: [TopDecl]
                     } deriving (Show,Generic,Data,Typeable)

-- | Top-level declarations.
data TopDecl = TDDecl Decl
             | TDData (Located DataDecl)
             | TDPrimType (Located PrimType)
             | TDPrimTerm (Located PrimTerm)
             | TDLocal (Located LocalDecls)
             | TDExport (Located (Exported [TopDecl]))
               deriving (Show,Generic,Data,Typeable)

-- | Declarations that are private to a group of top-declarations.
data LocalDecls = LocalDecls { ldLocals :: [Decl]
                             , ldDecls  :: [TopDecl]
                             } deriving (Show,Generic,Data,Typeable)

-- | Declarations that can show up anywhere.
data Decl = DBind (Located Bind)     -- ^ Name bindings
          | DSig (Located Signature) -- ^ Type signatures
          | DOpen (Located Open)     -- ^ Module imports
            deriving (Show,Generic,Data,Typeable)

-- | A module import.
data Open = Open { openMod     :: Located ModName
                 , openAs      :: Maybe (Located ModName)
                 , openHiding  :: Bool
                 , openSymbols :: [Located OpenSymbol]
                 } deriving (Show,Generic,Data,Typeable,Eq,Ord)

-- | Symbols that can be imported.
data OpenSymbol = OpenTerm String
                | OpenType String [String]
                  deriving (Show,Generic,Data,Typeable,Eq,Ord)

-- | Function binding.
data Bind = Bind { bindName   :: Located Name
                 , bindType   :: Maybe Schema
                 , bindBody   :: Match
                 } deriving (Show,Generic,Data,Typeable)

-- | A name with a signature.
data Signature = Signature { sigNames  :: [Located Name]
                           , sigSchema :: Schema
                           } deriving (Show,Generic,Data,Typeable)

-- | A primitive term name, with a signature.
data PrimTerm = PrimTerm { primTermName :: Name
                         , primTermType :: Schema
                         } deriving (Show,Generic,Data,Typeable)

-- | A primitive type, with a kind signature.
data PrimType = PrimType { primTypeName :: Name
                         , primTypeKind :: Kind
                         } deriving (Show,Generic,Data,Typeable)

-- | GADT declaration.
data DataDecl = DataDecl { dataName   :: Name
                         , dataArity  :: !Int
                         , dataGroups :: [Located ConstrGroup]
                         } deriving (Show,Generic,Data,Typeable)

-- | GADT constructor groups.
data ConstrGroup = ConstrGroup { groupResTys  :: [Type]
                               , groupConstrs :: [Located Constr]
                               } deriving (Show,Generic,Data,Typeable)

-- | GADT constructors.
data Constr = Constr { constrName   :: Name
                     , constrFields :: [Type]
                     } deriving (Show,Generic,Data,Typeable)

-- | Type schemas, with constraints.
data Schema = Forall [Prop] Type
              deriving (Show,Generic,Data,Typeable)

-- | Kinds and types use the same surface syntax.
type Kind = Type

type Prop = Type

data Type = TFun Type Type
          | TApp Type [Type]
          | TTuple [Type]
          | TCon Name
          | TVar Name
          | TRowExt (Labelled Type) Type
          | TEmptyRow
          | TLoc (Located Type)
            deriving (Show,Generic,Data,Typeable)

elimTFuns :: Type -> [Type]
elimTFuns ty = case ty of
  TFun l r -> l : elimTFuns r
  _        -> [ty]

elimTRowExt :: Type -> ([Labelled Type],Type)
elimTRowExt (TRowExt l r) = let (ls,r') = elimTRowExt r
                             in (l:ls,r')
elimTRowExt (TLoc l)      = elimTRowExt (unLoc l)
elimTRowExt r             = ([],r)

data Labelled a = Labelled { labName  :: Located Name
                           , labValue :: a
                           } deriving (Show,Generic,Data,Typeable,Functor
                                      ,Foldable,Traversable)

data Match = MPat   Pat   Match      -- ^ Pattern matching
           | MGuard Pat   Expr Match -- ^ Pattern guards
           | MSplit Match Match      -- ^ Choice
           | MSuccess Expr           -- ^ Body of a match
           | MFail                   -- ^ Unconditional failure
           | MLoc (Located Match)    -- ^ Source locations
             deriving (Show,Generic,Data,Typeable)

elimMSplits :: Match -> [Match]
elimMSplits (MSplit l r) = elimMSplits l ++ elimMSplits r
elimMSplits m            = [m]

flattenMPat :: Match -> ([Pat],Match)
flattenMPat  = go []
  where
  go acc (MPat p m) = go (p:acc) m
  go acc m          = (reverse acc, m)

-- | Nest a match in a sequence of patterns.
matchPats :: [Pat] -> Match -> Match
matchPats ps m = foldr MPat m ps

data Pat = PVar Name           -- ^ Variable introduction
         | PCon Name [Pat]     -- ^ Constructor patterns
         | PLit Literal        -- ^ Literal patterns
         | PWildcard           -- ^ The wildcard pattern
         | PLoc (Located Pat)  -- ^ Source location
           deriving (Show,Generic,Data,Typeable)

data Expr = Abs Match
          | App Expr [Expr]
          | Case Expr Match
          | Let [Decl] Expr
          | Var Name
          | Con Name
          | Lit Literal
          | ELoc (Located Expr)
            deriving (Show,Generic,Data,Typeable)

data Literal = LInt Integer Int
               deriving (Show,Generic,Data,Typeable)


-- Names -----------------------------------------------------------------------

instance Names Module where
  names f Module { .. } = Module modName <$> names f modDecls

instance Names Decl where
  names f (DBind lb) = DBind <$> names f lb
  names f (DSig ls)  = DSig  <$> names f ls
  names _ d@DOpen{}  = pure d

instance Names Literal where
  names = ignored

instance Names TopDecl
instance Names DataDecl
instance Names ConstrGroup
instance Names Constr
instance Names LocalDecls
instance Names Bind
instance Names Signature
instance Names PrimTerm
instance Names PrimType

instance Names Match
instance Names Pat
instance Names Expr

instance Names Schema
instance Names Type
instance Names a => Names (Labelled a)


-- Variable Binders-------------------------------------------------------------

instance BoundVars TopDecl

instance BoundVars DataDecl where
  boundVars DataDecl { .. } = Set.insert dataName (boundVars dataGroups)

instance BoundVars ConstrGroup where
  boundVars ConstrGroup { .. } = boundVars groupConstrs

instance BoundVars Constr where
  boundVars Constr { .. } = Set.singleton constrName

-- XXX this is a little strange
instance BoundVars LocalDecls where
  boundVars LocalDecls { .. } = boundVars ldDecls

instance BoundVars PrimTerm where
  boundVars PrimTerm { .. } = Set.singleton primTermName

instance BoundVars PrimType where
  boundVars PrimType { .. } = Set.singleton primTypeName

-- only bindings can bind variables
instance BoundVars Decl where
  boundVars (DBind lb) = boundVars lb
  boundVars (DSig ls)  = boundVars ls
  boundVars DOpen{}    = Set.empty

instance BoundVars Bind where
  boundVars Bind { .. } = Set.singleton (unLoc bindName)

instance BoundVars Signature where
  boundVars Signature { .. } = Set.fromList [ unLoc ln | ln <- sigNames ]

instance BoundVars Pat where
  boundVars (PVar n)    = Set.singleton n
  boundVars (PCon _ ps) = boundVars ps
  boundVars PWildcard   = Set.empty
  boundVars PLit{}      = Set.empty
  boundVars (PLoc lp)   = boundVars lp


-- Free Variables --------------------------------------------------------------

instance FreeVars Signature
instance FreeVars a => FreeVars (Labelled a)
instance FreeVars Schema

instance FreeVars Module where
  freeVars Module { .. } = freeVars modDecls Set.\\ boundVars modDecls

instance FreeVars TopDecl

-- XXX not really sure how this should be dealt with...
instance FreeVars LocalDecls where
  freeVars LocalDecls { .. } =
    freeVars (ldLocals,ldDecls) Set.\\ boundVars (ldLocals,ldDecls)

instance FreeVars DataDecl where
  freeVars DataDecl { .. } = Set.delete dataName (freeVars dataGroups)

instance FreeVars ConstrGroup

instance FreeVars Constr where
  freeVars Constr { .. } = Set.delete constrName (freeVars constrFields)

instance FreeVars PrimTerm where
  freeVars PrimTerm { .. } = freeVars primTermType

instance FreeVars PrimType where
  freeVars PrimType { .. } = freeVars primTypeKind

instance FreeVars Decl where
  freeVars (DBind lb) = freeVars lb
  freeVars (DSig ls)  = freeVars ls
  freeVars DOpen{}    = Set.empty

instance FreeVars Bind where
  freeVars Bind { .. } = freeVars (bindBody,bindType)

instance FreeVars Pat where
  freeVars (PCon c ps) = Set.insert c (freeVars ps)
  freeVars (PVar _)    = Set.empty
  freeVars PWildcard   = Set.empty
  freeVars PLit{}      = Set.empty
  freeVars (PLoc lp)   = boundVars lp

instance FreeVars Match where
  freeVars (MPat p m')     = freeVars (p,m')   Set.\\ boundVars p
  freeVars (MGuard p e m') = freeVars (p,e,m') Set.\\ boundVars p
  freeVars (MSplit l r)    = freeVars (l,r)
  freeVars (MSuccess e)    = freeVars e
  freeVars MFail           = Set.empty
  freeVars (MLoc lm)       = freeVars lm

instance FreeVars Expr where
  freeVars (Abs m)    = freeVars m
  freeVars (Case e m) = freeVars (e,m)
  freeVars (Let ds e) = freeVars (ds,e) Set.\\ boundVars ds
  freeVars (App e es) = freeVars (e,es)
  freeVars (Var n)    = Set.singleton n
  freeVars (Con n)    = Set.singleton n
  freeVars Lit{}      = Set.empty
  freeVars (ELoc le)  = freeVars le

instance FreeVars Type


-- Pretty Printing -------------------------------------------------------------


instance Pretty Module where
  ppr m = pp Kmodule
      <+> ppModName (unLoc (modName m))
      <+> pp Kwhere
       $$ layout (map pp (modDecls m))

instance Pretty TopDecl where
  ppr td = case td of
    TDDecl d      -> ppr d
    TDData d      -> ppr d
    TDPrimType pt -> ppr pt
    TDPrimTerm pt -> ppr pt
    TDLocal ls    -> ppr ls
    TDExport ds   -> ppExported (layout . map pp) (unLoc ds)

instance Pretty LocalDecls where
  ppr ls = hang (ppr Klocal)       6 (layout (map pp (ldLocals ls)))
        $$ hang (nest 3 (ppr Kin)) 6 (layout (map pp (ldDecls  ls)))

instance Pretty Decl where
  ppr d = case d of
    DBind b -> ppr b
    DSig s  -> ppr s
    DOpen o -> ppr o

instance Pretty DataDecl where
  ppr DataDecl { .. } = pp Kdata
                    <+> vcat (map (ppConstrGroup dataName) dataGroups)

ppConstrGroup :: Name -> Located ConstrGroup -> PPDoc
ppConstrGroup n lcg =
  pp n <+> hsep (map (ppPrec 10) (groupResTys cg))
       <+> snd (foldl ppConstr (pp Kassign, empty) (groupConstrs cg))
  where
  cg                  = unLoc lcg
  cp                  = pp Kpipe
  ppConstr (p,acc) lc =
    (cp,acc $$ hsep (p:pp (constrName c):map (ppPrec 10) (constrFields c)))
    where
    c = unLoc lc

instance Pretty PrimType where
  ppr pt = pp Kprimitive <+> fsep [ pp (primTypeName pt)
                                  , pp Kcolon
                                  , pp (primTypeKind pt) ]

instance Pretty PrimTerm where
  ppr pt = pp Kprimitive <+> fsep [ pp (primTermName pt)
                                  , pp Kcolon
                                  , pp (primTermType pt) ]

instance Pretty Bind where
  ppr b = vcat (map ppEqn (elimMSplits (bindBody b)))
    where
    ppEqn m = pp (bindName b) <+> fsep (ppDef m)

-- | Pretty-print a match as the body of a binding.
ppDef :: Match -> [PPDoc]
ppDef m = map (ppPrec 10) ps ++ [ pp Kassign, pp body ]
  where
  (ps,body) = flattenMPat m

instance Pretty Match where
  ppr m = case m of
    MPat p m'     -> fsep [ pp p <+> pp Krarrow, pp m' ]
    MGuard p e m' -> fsep [ pp p <+> pp Klarrow <+> pp e
                          , char ',' <+> pp m' ]
    MSplit l r    -> ppr l $$ ppr r
    MSuccess e    -> ppr e
    MFail         -> empty
    MLoc lm       -> ppr (unLoc lm)

instance Pretty Pat where
  ppr pat = case pat of
    PVar v    -> pp v
    PCon c [] -> pp c
    PCon c ps -> optParens 10 (fsep (pp c : map (ppPrec 10) ps))
    PWildcard -> char '_'
    PLit l    -> ppr l
    PLoc lp   -> ppr (unLoc lp)

instance Pretty Expr where
  ppr (Abs m)    = optParens 1 (pp Klambda <> ppr m)

  ppr (App f xs) = optParens 10 (pp f <+> nest 2 (fsep (map( ppPrec 10) xs)))

  ppr (Case e m) = hang (pp Kcase <+> pp e <+> pp Kof)
                      2 (ppArms m)

  ppr (Let bs e) = optParens 1 $ hang (pp Klet) 4 (layout (map pp bs))
                              $$ hang (nest 1 (pp Kin)) 4 (pp e)

  ppr (ELoc le)  = ppr le
  ppr (Var n)    = pp n
  ppr (Con n)    = pp n
  ppr (Lit l)    = pp l

ppArms :: Match -> PPDoc
ppArms m = layout (map pp (elimMSplits m))

instance Pretty Signature where
    ppr sig = hang (fsep (commas (map pp (sigNames sig))) <+> pp Kcolon)
                 2 (pp (sigSchema sig))

instance Pretty Open where
  ppr o = hang (pp Kopen <+> ppModName (unLoc (openMod o)) <+> altName)
             5 spec
    where
    altName = case openAs o of
      Nothing -> empty
      Just ln -> pp Kas <+> ppModName (unLoc ln)

    spec | openHiding o = pp Khiding <+> symbols
         | otherwise    = symbols

    symbols | null (openSymbols o) = empty
            | otherwise            = parens (fsep (commas (map pp (openSymbols o))))

instance Pretty OpenSymbol where
  ppr os = case os of
    OpenTerm n      -> text n
    OpenType n cons -> text n <> parens (fsep (commas (map text cons)))

instance Pretty Schema where
  ppr (Forall ps ty)
    | null ps   = pp ty
    | otherwise = sep [ props, pp ty ]
      where
      props = case ps of
        []     -> empty
        [prop] -> pp prop <+> pp KRarrow
        _      -> parens (fsep (commas (map pp ps))) <+> pp KRarrow

ppLabelled :: Pretty a => PPDoc -> Labelled a -> PPDoc
ppLabelled p l = sep [ pp (labName l) <+> p, pp (labValue l) ]

instance Pretty Type where
  ppr ty = case ty of
    TFun{}    -> optParens 10 $ fsep
                              $ intersperse (pp Krarrow)
                              $ map (ppPrec 10) (elimTFuns ty)
    TApp f xs -> optParens 10 (fsep (pp f : map (ppPrec 10) xs))
    TTuple ts -> parens (fsep (commas (map pp ts)))
    TCon n    -> pp n
    TVar n    -> pp n
    TRowExt{} -> ppRowExt ty
    TEmptyRow -> braces empty
    TLoc lt   -> ppr lt

ppRowExt :: Type -> PPDoc
ppRowExt ty = pp Klbrace
           <> fsep (commas (map (ppLabelled (pp Kcolon)) ls) ++ [row])
           <> pp Krbrace
  where
  (ls,r) = elimTRowExt ty

  row = case stripLoc r of
    TEmptyRow -> empty
    _         -> pp Kpipe <+> pp r


instance Pretty Literal where
  ppr lit = case lit of
    LInt i _ -> withGraphics [fg magenta, bold] (integer i)


-- Location Information --------------------------------------------------------

instance HasLocation TopDecl where
  getLoc td = case td of
    TDDecl d      -> getLoc d
    TDData d      -> getLoc d
    TDPrimType pt -> getLoc pt
    TDPrimTerm pt -> getLoc pt
    TDLocal ls    -> getLoc ls
    TDExport ds   -> getLoc ds

  stripLoc td = case td of
    TDDecl d      -> TDDecl (stripLoc d)
    TDData d      -> TDData (stripLoc d)
    TDPrimType pt -> TDPrimType (stripLoc pt)
    TDPrimTerm pt -> TDPrimTerm (stripLoc pt)
    TDLocal ls    -> TDLocal (stripLoc ls)
    TDExport ds   -> TDExport (stripLoc ds)

instance HasLocation Decl where
  getLoc d = case d of
    DBind l -> getLoc l
    DSig  l -> getLoc l
    DOpen l -> getLoc l

  stripLoc d = case d of
    DBind l -> DBind (stripLoc l)
    DSig  l -> DSig  (stripLoc l)
    DOpen l -> DOpen (stripLoc l)

instance HasLocation Expr where
  getLoc e = case e of
    ELoc le -> getLoc le
    _       -> mempty

  stripLoc (ELoc l)   = unLoc l
  stripLoc (Abs m)    = Abs  (stripLoc m)
  stripLoc (Case l r) = Case (stripLoc l) (stripLoc r)
  stripLoc (Let b e)  = Let  (stripLoc b) (stripLoc e)
  stripLoc (App f xs) = App  (stripLoc f) (stripLoc xs)
  stripLoc e          = e

instance HasLocation Match where
  getLoc m = case m of
    MLoc lm -> getLoc lm
    _       -> mempty

  stripLoc (MLoc l)       = unLoc l
  stripLoc (MPat   p m)   = MPat     (stripLoc p) (stripLoc m)
  stripLoc (MGuard p e m) = MGuard   (stripLoc p) (stripLoc e) (stripLoc m)
  stripLoc (MSplit l r)   = MSplit   (stripLoc l) (stripLoc r)
  stripLoc (MSuccess e)   = MSuccess (stripLoc e)
  stripLoc MFail          = MFail

instance HasLocation Pat where
  getLoc pat = case pat of
    PLoc lp -> getLoc lp
    _       -> mempty

  stripLoc (PLoc l)    = unLoc l
  stripLoc (PCon n ps) = PCon n (stripLoc ps)
  stripLoc p           = p

instance HasLocation Schema where
  getLoc   (Forall ps ty) = getLoc ps `mappend` getLoc ty
  stripLoc (Forall ps ty) = Forall (stripLoc ps) (stripLoc ty)

instance HasLocation Type where
  getLoc ty = case ty of
    TLoc lt -> getLoc lt
    _       -> mempty

  stripLoc (TLoc lt)     = unLoc lt
  stripLoc (TFun a b)    = TFun    (stripLoc a) (stripLoc b)
  stripLoc (TApp f xs)   = TApp    (stripLoc f) (stripLoc xs)
  stripLoc (TTuple ts)   = TTuple  (stripLoc ts)
  stripLoc (TRowExt l t) = TRowExt (stripLoc l) (stripLoc t)
  stripLoc ty            = ty

instance HasLocation a => HasLocation (Labelled a) where
  getLoc   l = getLoc (labName l) `mappend` getLoc (labValue l)
  stripLoc l = Labelled { labName  = stripLoc (labName l)
                        , labValue = stripLoc (labValue l) }
