{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Syntax.ParserCore where

import Dang.ModuleSystem.Export (Export(..))
import Dang.QualName
import Dang.Syntax.AST
import Dang.Syntax.Layout (layout)
import Dang.Syntax.Lexeme
import Dang.Syntax.Lexer (scan)
import Dang.Syntax.Renumber (renumber)
import Dang.TypeChecker.Types
import Dang.TypeChecker.Unify
import Dang.Utils.Location (Range)
import Data.ClashMap as CM

import Control.Applicative (Applicative)
import Data.List (nub)
import Data.Monoid (Monoid(..))
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Loc(loc_filename),location)
import MonadLib
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L


-- Lexer/Parser Monad ----------------------------------------------------------

data Error = Error String Range deriving Show

data ParserState = ParserState
  { psTokens  :: [Lexeme]
  } deriving Show

initParserState :: [Lexeme] -> ParserState
initParserState ls = ParserState
  { psTokens  = ls
  }

newtype Parser a = Parser
  { unParser :: WriterT [ParserError]
              (StateT ParserState (ExceptionT Error Id)) a
  } deriving (Functor,Applicative,Monad)

data ParserError
  = MultipleDefs Name
  | NoBinding Name
    deriving (Show)

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser Error where
  raise = Parser . raise

instance RunExceptionM Parser Error where
  try m = Parser (try (unParser m))

instance WriterM Parser [ParserError] where
  put = Parser . put

instance RunWriterM Parser [ParserError] where
  collect = Parser . collect . unParser

-- | Raise an exception from the parser.
raiseP :: String -> Range -> Parser a
raiseP msg pos = raise (Error msg pos)

-- | Run the parser over the file given.
runParser :: [Lexeme] -> Parser a -> Either Error a
runParser ls m =
  case runM (unParser body) (initParserState ls) of
    Right ((a,_),_) -> Right a
    Left err        -> Left err
  where
  body = do
    (res,errs) <- collect m
    unless (null errs)
        (raiseP ("definition errors: " ++ show errs) mempty)
    return res

-- | Run the parser over a string in the Q monad.
runParserQ :: Parser a -> String -> Q a
runParserQ m str = do
  loc <- location
  let tokens = layout (scan (loc_filename loc) (L.pack str))
  case runParser tokens m of
    Right a            -> return a
    Left (Error err _) -> fail err


-- Parsed Syntax ---------------------------------------------------------------

type NameMap = CM.ClashMap Name

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

-- | Merge type and untyped declarations into a typed declaration, otherwise
-- generate a name clash.
resolveTypes :: Strategy PDecl
resolveTypes a b = case (a,b) of
  (DeclTerm u, DeclType ty) -> ok (DeclTyped (mkTypedDecl u ty))
  (DeclType ty, DeclTerm u) -> ok (DeclTyped (mkTypedDecl u ty))
  _                         -> clash a b


-- | Generate a term declaration that's lacking a type binding.
mkUntyped :: UntypedDecl -> PDecls
mkUntyped d = mempty { parsedPDecls = singleton (untypedName d) (DeclTerm d) }

-- | Generate a declaration of a type for a term.
mkTypeDecl :: Name -> Scheme -> PDecls
mkTypeDecl n t = mempty { parsedPDecls = singleton n (DeclType t) }

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


addDecl :: UntypedDecl -> PDecls -> PDecls
addDecl d ds = ds
  { parsedPDecls = CM.insertWith resolveTypes (untypedName d) (DeclTerm d)
      (parsedPDecls ds)
  }

mkDecls :: [UntypedDecl] -> PDecls
mkDecls ds =  mempty { parsedPDecls = foldl step CM.empty ds }
  where
  step m d = CM.insertWith resolveTypes (untypedName d) (DeclTerm d) m

exportBlock :: Export -> PDecls -> PDecls
exportBlock ex pds = pds { parsedPDecls = f `fmap` parsedPDecls pds }
  where
  f (DeclTerm d)  = DeclTerm d { untypedExport = ex }
  f (DeclTyped d) = DeclTyped d { typedExport = ex }
  f e             = e

mkOpen :: Open -> PDecls
mkOpen o = mempty { parsedOpens = [o] }

mkPrimTerm :: PrimTerm -> PDecls
mkPrimTerm d = mempty { parsedPrimTerms = singleton (primTermName d) d }

mkPrimType :: PrimType -> PDecls
mkPrimType d = mempty { parsedPrimTypes = singleton (primTypeName d) d }

mkDataDecl :: Range -> [(Name,Forall ConstrGroup)] -> Parser PDecls
mkDataDecl range groups = do
  let (ns,qgs) = unzip groups
  when (null groups) (raiseP "No types defined by data declaration" range)
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

groupName :: Range -> [Name] -> Parser Name
groupName range ns =
  case nub ns of
    [n] -> return n
    _   -> raiseP "Type names don't agree" range

groupArity :: Range -> [Forall ConstrGroup] -> Parser Int
groupArity range gs =
  case nub (map arity gs) of
    [n] -> return n
    _   -> raiseP "Type group arities don't agree" range
  where
  arity = length . groupArgs . forallData

resolveNamed :: NameMap a -> Parser [a]
resolveNamed nm = do
  let (oks,clashes) = CM.foldClashMap step ([],[]) nm
      step n c (as,bs) = case clashElems c of
        [a] -> (a:as,bs)
        _es -> (as,MultipleDefs n:bs)
  put clashes
  return oks

processBindings :: NameMap PDecl -> Parser ([TypedDecl],[UntypedDecl])
processBindings ds = do
  let (typed,untyped,clashes) = CM.foldClashMap step ([],[],[]) ds
      step n c (ts,us,bs) = case clashElems c of
        [DeclTyped t]  -> (t:ts,us,bs)
        [DeclTerm u]   -> (ts,u:us,bs)
        [DeclType _ty] -> (ts,us,NoBinding n:bs)
        _es            -> (ts,us,MultipleDefs n:bs)
  put clashes
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
