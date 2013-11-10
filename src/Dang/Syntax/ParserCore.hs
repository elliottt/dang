{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Syntax.ParserCore where

import Dang.Monad ( Error(..) )
import Dang.ModuleSystem.Export (Export(..))
import Dang.ModuleSystem.QualName
import Dang.Syntax.AST
import Dang.Syntax.Layout (layout)
import Dang.Syntax.Lexeme
import Dang.Syntax.Lexer (scan)
import Dang.Syntax.Renumber (renumber)
import Dang.TypeChecker.Types
import Dang.TypeChecker.Unify
import Dang.Utils.Location (SrcLoc,ppLoc,Located,at,unLoc,HasLocation(..))
import Dang.Utils.Pretty
import Data.ClashMap as CM

import Control.Applicative (Applicative)
import Data.List (nub)
import Data.Monoid (Monoid(..))
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Loc(loc_filename),location)
import MonadLib
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L


-- Definition Errors -----------------------------------------------------------

data DefError
  = MultipleDefs (Located Name) [SrcLoc]
  | NoBinding (Located Name)
    deriving (Show)

fromDefError :: DefError -> Error
fromDefError err = case err of

  MultipleDefs n locs -> Error loc msg
    where
    loc = case locs of
      l:_ -> l
      []  -> mempty
    msg = text "Multiple declarations of" <> quoted (ppName (unLoc n))
       $$ text "Declared at:" <+> nest 0 (vcat (map ppLoc locs))

  NoBinding ln -> Error (getLoc ln) msg
    where
    msg = sep [ text "The type signature for" <+> quoted (ppName (unLoc ln))
              , text "lacks an accompanying binding" ]


-- Lexer/Parser Monad ----------------------------------------------------------

data ParserState = ParserState
  { psTokens  :: [Lexeme]
  } deriving Show

initParserState :: [Lexeme] -> ParserState
initParserState ls = ParserState
  { psTokens  = ls
  }

newtype Parser a = Parser
  { unParser :: WriterT [DefError]
              (StateT ParserState (ExceptionT [Error] Id)) a
  } deriving (Functor,Applicative,Monad)

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

-- | Report some parser errors.
report :: [DefError] -> Parser ()
report errs = Parser (put errs)

-- | Collect definition errors in a parser action.
defErrors :: Parser a -> Parser (a,[DefError])
defErrors m = Parser (collect (unParser m))

raiseError :: [Error] -> Parser a
raiseError errs = Parser (raise errs)

-- | Unrecoverable parser error.
parseError :: SrcLoc -> Doc -> Parser a
parseError range doc = Parser (raise [Error range doc])

-- | Run the parser over the file given.
runParser :: [Lexeme] -> Parser a -> Either [Error] a
runParser ls m =
  case runM (unParser body) (initParserState ls) of
    Right ((res,_),_) -> Right res
    Left errs         -> Left errs
  where
  body = do
    (res,errs) <- Parser (collect (unParser m))
    unless (null errs) (raiseError (map fromDefError errs))
    return res

-- | Run the parser over a string in the Q monad.
runParserQ :: Parser a -> String -> Q a
runParserQ m str = do
  loc <- location
  let tokens = layout (scan (loc_filename loc) (L.pack str))
  case runParser tokens m of
    Right a   -> return a
    Left errs -> fail (render (vcat (map ppr errs)))


-- Parsed Syntax ---------------------------------------------------------------

type NameMap = CM.ClashMap LName

-- | A collection of parsed declarations.  Loosely, this is a module.
data PDecls = PDecls
  { parsedPDecls    :: NameMap PDecl
  , parsedOpens     :: [Open]
  , parsedPrimTerms :: NameMap PrimTerm
  , parsedPrimTypes :: NameMap PrimType
  , parsedDataDecls :: [DataDecl]
  } deriving (Show)

instance Monoid PDecls where
  mempty = PDecls
    { parsedPDecls    = CM.empty
    , parsedOpens     = []
    , parsedPrimTerms = CM.empty
    , parsedPrimTypes = CM.empty
    , parsedDataDecls = []
    }

  mappend ds1 ds2 = PDecls
    { parsedPDecls    = merge resolveTypes parsedPDecls
    , parsedOpens     = parsedOpens ds1 ++ parsedOpens ds2
    , parsedPrimTerms = merge clash parsedPrimTerms
    , parsedPrimTypes = merge clash parsedPrimTypes
    , parsedDataDecls = parsedDataDecls ds1 ++ parsedDataDecls ds2
    }
    where
    merge strat prj = CM.unionWith strat (prj ds1) (prj ds2)

-- | Term declarations can be in three stages during parsing: just a body, just
-- a type, or both a type and a term.  This type captures those states.
data PDecl
  = DeclTerm UntypedDecl
  | DeclType Scheme
  | DeclTyped TypedDecl
    deriving (Show)

instance HasLocation PDecl where
  getLoc pd = case pd of
    DeclTerm lu  -> getLoc lu
    DeclType ls  -> getLoc ls
    DeclTyped lt -> getLoc lt


-- | Merge type and untyped declarations into a typed declaration, otherwise
-- generate a name clash.
resolveTypes :: Strategy PDecl
resolveTypes a b = case (a,b) of
  (DeclTerm u, DeclType ty) -> ok (DeclTyped (mkTypedDecl u ty))
  (DeclType ty, DeclTerm u) -> ok (DeclTyped (mkTypedDecl u ty))
  _                         -> clash a b


-- | Generate a term declaration that's lacking a type binding.
mkUntyped :: Located UntypedDecl -> PDecls
mkUntyped ld = mempty
  { parsedPDecls = singleton (untypedName `fmap` ld) (DeclTerm (unLoc ld))
  }

-- | Generate a declaration of a type for a term.
mkTypeDecl :: SrcLoc -> Name -> Scheme -> PDecls
mkTypeDecl range n t = mempty
  { parsedPDecls = singleton (n `at` range) (DeclType t)
  }

-- | Quantify all free variables in a parsed type.
mkScheme :: Context -> Type -> Scheme
mkScheme cxt ty = quantify (Set.toList (typeVars ty')) (Qual cxt ty')
  where
  ty' = renumber ty

mkConstrGroup :: [Type] -> [Constr] -> Forall ConstrGroup
mkConstrGroup args cs = quantify vars ConstrGroup
  { groupArgs    = args'
  , groupConstrs = renumber cs
  }
  where
  args' = renumber args
  vars  = Set.toList (typeVars args')


addDecl :: Located UntypedDecl -> PDecls -> PDecls
addDecl ld ds = ds
  { parsedPDecls =
      CM.insertWith resolveTypes lname (DeclTerm udecl) (parsedPDecls ds)
  }
  where
  udecl = unLoc ld
  lname = untypedName `fmap` ld

mkDecls :: [Located UntypedDecl] -> PDecls
mkDecls  = foldr addDecl mempty

exportBlock :: Export -> PDecls -> PDecls
exportBlock ex pds = pds { parsedPDecls = f `fmap` parsedPDecls pds }
  where
  f (DeclTerm d)  = DeclTerm  d { untypedExport = ex }
  f (DeclTyped d) = DeclTyped d { typedExport   = ex }
  f e             = e

mkOpen :: Located Open -> PDecls
mkOpen o = mempty { parsedOpens = [unLoc o] }

mkPrimTerm :: SrcLoc -> PrimTerm -> PDecls
mkPrimTerm range d = mempty
  { parsedPrimTerms = singleton (primTermName d `at` range) d
  }

mkPrimType :: SrcLoc -> PrimType -> PDecls
mkPrimType range d = mempty
  { parsedPrimTypes = singleton (primTypeName d `at` range) d
  }

mkDataDecl :: SrcLoc -> [(Name,Forall ConstrGroup)] -> Parser PDecls
mkDataDecl range groups = do
  let (ns,qgs) = unzip groups
  when (null groups)
      (parseError range (text "No types defined by data declaration"))
  n <- groupName range ns
  a <- groupArity range qgs
  let d = DataDecl
        { dataName   = n
        , dataArity  = a
        , dataKind   = setSort
        , dataExport = Public
        , dataGroups = qgs
        }
  return mempty { parsedDataDecls = [d] }

groupName :: SrcLoc -> [Name] -> Parser Name
groupName range ns =
  case nub ns of
    [n] -> return n
    _   -> parseError range (text "Type names don't agree")

groupArity :: SrcLoc -> [Forall ConstrGroup] -> Parser Int
groupArity range gs =
  case nub (map arity gs) of
    [n] -> return n
    _   -> parseError range (text "Type group arities don't agree")
  where
  arity = length . groupArgs . forallData

resolveNamed :: HasLocation a => NameMap a -> Parser [a]
resolveNamed nm = do
  let (oks,clashes) = CM.foldClashMap step ([],[]) nm
      step n c (as,bs) = case clashElems c of
        [a] -> (a:as,bs)
        es  -> (as,MultipleDefs n (map getLoc es):bs)
  report clashes
  return oks

processBindings :: NameMap PDecl -> Parser ([TypedDecl],[UntypedDecl])
processBindings ds = do
  let (typed,untyped,clashes) = CM.foldClashMap step ([],[],[]) ds
      step n c (ts,us,bs) = case clashElems c of
        [DeclTyped t]  -> (t:ts,us,bs)
        [DeclTerm u]   -> (ts,u:us,bs)
        [DeclType _ty] -> (ts,us,NoBinding n:bs)
        es             -> (ts,us,MultipleDefs n (map getLoc es):bs)
  report clashes
  return (typed,untyped)

-- | Make a module from a set of parsed declarations, and a name.
mkModule :: QualName -> PDecls -> Parser Module
mkModule qn pds = do
  (ts,us) <- processBindings (parsedPDecls pds)
  tms     <- resolveNamed (parsedPrimTerms pds)
  tys     <- resolveNamed (parsedPrimTypes pds)
  return Module
    { modName      = qn
    , modOpens     = parsedOpens pds
    , modTyped     = ts
    , modUntyped   = us
    , modPrimTerms = tms
    , modPrimTypes = tys
    , modDatas     = parsedDataDecls pds
    }
