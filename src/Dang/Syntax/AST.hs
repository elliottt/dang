{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Dang.Syntax.AST where

import Dang.Syntax.Lexeme (Keyword(..))
import Dang.ModuleSystem.Export ( Export(..) )
import Dang.ModuleSystem.QualName
import Dang.Utils.Location
import Dang.Utils.Pretty
import Dang.Variables

import           Data.Data ( Data )
import           Data.Foldable ( Foldable )
import           Data.List ( intersperse )
import           Data.Monoid ( mempty, mappend )
import qualified Data.Set as Set
import           Data.Traversable ( Traversable )
import           Data.Typeable ( Typeable )


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
             | BEmpty
               -- ^ No declarations.
             | BLoc (Located (Block a))
               -- ^ Source lcoations attached to a block of declaration.
               deriving (Show,Data,Typeable,Functor,Foldable,Traversable)

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
             | TDData (Located DataDecl)
             | TDPrimType (Located PrimType)
             | TDPrimTerm (Located PrimTerm)
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
data Bind = Bind { bindName   :: Located Name
                 , bindType   :: Maybe Schema
                 , bindBody   :: Match
                 } deriving (Show,Data,Typeable)

-- | A name with a signature.
data Signature = Signature { sigNames  :: [Located Name]
                           , sigSchema :: Schema
                           } deriving (Show,Data,Typeable)

-- | A primitive term name, with a signature.
data PrimTerm = PrimTerm { primTermName :: Name
                         , primTermType :: Schema
                         } deriving (Show,Data,Typeable)

-- | A primitive type, with a kind signature.
data PrimType = PrimType { primTypeName :: Name
                         , primTypeKind :: Kind
                         } deriving (Show,Data,Typeable)

-- | GADT declaration.
data DataDecl = DataDecl { dataName   :: Name
                         , dataArity  :: !Int
                         , dataGroups :: [Located ConstrGroup]
                         } deriving (Show,Data,Typeable)

-- | GADT constructor groups.
data ConstrGroup = ConstrGroup { groupResTys  :: [Type]
                               , groupConstrs :: [Located Constr]
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
          | TRowExt (Labelled Type) Type
          | TEmptyRow
          | TLoc (Located Type)
            deriving (Show,Data,Typeable)

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
                           } deriving (Show,Data,Typeable,Functor,Foldable
                                      ,Traversable)

data Match = MPat   Pat   Match      -- ^ Pattern matching
           | MGuard Pat   Expr Match -- ^ Pattern guards
           | MSplit Match Match      -- ^ Choice
           | MSuccess Expr           -- ^ Body of a match
           | MFail                   -- ^ Unconditional failure
           | MLoc (Located Match)    -- ^ Source locations
             deriving (Show,Data,Typeable)

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
           deriving (Show,Data,Typeable)

data Expr = Abs Match
          | App Expr [Expr]
          | Case Expr Match
          | Let (Block Decl) Expr
          | Var Name
          | Con Name
          | Lit Literal
          | ELoc (Located Expr)
            deriving (Show,Data,Typeable)

data Literal = LInt Integer Int
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
    BLoc lb      -> boundVars lb
    BEmpty       -> Set.empty

-- only bindings can bind variables
instance BoundVars Decl where
  boundVars d = case d of
    DBind b -> boundVars b
    DSig _  -> Set.empty
    DOpen _ -> Set.empty

instance BoundVars Bind where
  boundVars b = Set.singleton (unLoc (bindName b))

instance BoundVars Signature where
  boundVars sig = Set.fromList (map unLoc (sigNames sig))

instance BoundVars Pat where
  boundVars p = case p of
    PVar n     -> Set.singleton n
    PCon qn ps -> Set.insert qn (boundVars ps)
    PWildcard  -> Set.empty
    PLit _     -> Set.empty
    PLoc lp    -> boundVars lp


-- Free Variables --------------------------------------------------------------

instance (BoundVars a, FreeVars a) => FreeVars (Block a) where
  freeVars b = case b of
    BSingle a    -> freeVars a
    BExport _ b' -> freeVars b'
    BRec b'      -> freeVars b'
    BComb l r    -> freeVars [l,r]  Set.\\ boundVars [l,r]
    BSeq l r     -> freeVars [l,r]  Set.\\ boundVars l
    BLocal l b'  -> freeVars [l,b'] Set.\\ boundVars l
    BLoc lb      -> freeVars lb
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
    MPat p m'     -> freeVars m'     Set.\\ boundVars p
    MGuard p e m' -> freeVars (e,m') Set.\\ boundVars p
    MSplit l r    -> freeVars [l,r]
    MSuccess e    -> freeVars e
    MFail         -> Set.empty
    MLoc lm       -> freeVars lm

instance FreeVars Expr where
  freeVars tm = case tm of
    Abs m    -> freeVars m
    Case e m -> freeVars e `Set.union` freeVars m
    Let b e  -> freeVars (b,e) Set.\\ boundVars b
    App f xs -> freeVars f `Set.union` freeVars xs
    Var n    -> Set.singleton n
    Con n    -> Set.singleton n
    Lit _    -> Set.empty
    ELoc le  -> freeVars le

instance FreeVars a => FreeVars (Labelled a) where
  freeVars a = freeVars (labValue a)

instance FreeVars Type where
  freeVars (TFun a b)     = freeVars (a,b)
  freeVars (TApp f xs)    = freeVars (f,xs)
  freeVars (TTuple ts)    = freeVars ts
  freeVars (TCon c)       = Set.singleton c
  freeVars (TVar _)       = Set.empty
  freeVars (TRowExt ls r) = freeVars (ls,r)
  freeVars TEmptyRow      = Set.empty
  freeVars (TLoc lt)      = freeVars lt


-- Pretty Printing -------------------------------------------------------------


instance Pretty Module where
  ppr m = pp Kmodule <+> ppModName (unLoc (modName m)) <+> pp Kwhere
       $$ pp (modDecls m)

instance Pretty a => Pretty (Block a) where
  ppr b = case b of
    BSingle a    -> ppr a
    BComb{}      -> layout (map pp (elimBCombs b))
    BExport e b' -> hang (pp e) 2 (pp b')
    BRec b'      -> hang (pp Krec) 2 (pp b')
    BSeq{}       -> layout (map pp (elimBSeqs b))
    BLocal as bs -> hang (pp Klocal) 6 (pp as)
                 $$ hang (nest 3 (pp Kin)) 6 (pp bs)
    BEmpty       -> empty
    BLoc lb      -> ppr lb

instance Pretty TopDecl where
  ppr td = case td of
    TDDecl d      -> ppr d
    TDData d      -> ppr d
    TDPrimType pt -> ppr pt
    TDPrimTerm pt -> ppr pt

instance Pretty Decl where
  ppr d = case d of
    DBind b -> ppr b
    DSig s  -> ppr s
    DOpen o -> ppr o

instance Pretty DataDecl where
  ppr d = pp Kdata <+> vcat (map (ppConstrGroup (dataName d)) (dataGroups d))

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

  ppr (Let b e)  = optParens 1 $ hang (pp Klet) 4 (pp b)
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

instance HasLocation a => HasLocation (Block a) where
  getLoc b = case b of
    BLoc lb -> getLoc lb
    _       -> mempty

  stripLoc (BLoc l)      = unLoc l
  stripLoc (BExport e b) = BExport e (stripLoc b)
  stripLoc (BRec b)      = BRec      (stripLoc b)
  stripLoc (BComb l r)   = BComb     (stripLoc l) (stripLoc r)
  stripLoc (BSeq  l r)   = BSeq      (stripLoc l) (stripLoc r)
  stripLoc (BLocal l r)  = BLocal    (stripLoc l) (stripLoc r)
  stripLoc b             = b


instance HasLocation TopDecl where
  getLoc td = case td of
    TDDecl d      -> getLoc d
    TDData d      -> getLoc d
    TDPrimType pt -> getLoc pt
    TDPrimTerm pt -> getLoc pt

  stripLoc td = case td of
    TDDecl d      -> TDDecl (stripLoc d)
    TDData d      -> TDData (stripLoc d)
    TDPrimType pt -> TDPrimType (stripLoc pt)
    TDPrimTerm pt -> TDPrimTerm (stripLoc pt)

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
