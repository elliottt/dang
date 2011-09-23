{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax.ParserCore where

import Data.ClashMap as CM
import QualName
import Syntax.AST
import TypeChecker.Types
import TypeChecker.Unify

import Control.Applicative (Applicative)
import Control.Monad.ST.Strict (runST)
import Data.Monoid (Monoid(..))
import Data.STRef.Strict (newSTRef,readSTRef,writeSTRef)
import MonadLib
import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Lexer/Parser Monad ----------------------------------------------------------

data Position = Position
  { posOff  :: !Int
  , posLine :: !Int
  , posCol  :: !Int
  , posFile :: FilePath
  } deriving Show

initPosition :: FilePath -> Position
initPosition path = Position
  { posOff  = 0
  , posLine = 1
  , posCol  = 1
  , posFile = path
  }

nullPosition :: Position
nullPosition  = Position
  { posOff  = 0
  , posLine = 1
  , posCol  = 1
  , posFile = "<unknown>"
  }

movePos :: Position -> Char -> Position
movePos (Position a line col path) c =
  case c of
    '\t' -> Position (a+1) line (col+8) path
    '\n' -> Position (a+1) (line+1) 1 path
    _    -> Position (a+1) line (col+1) path

data Token
  = TReserved String
  | TConIdent String
  | TSymIdent String
  | TOperIdent String
  | TInt Integer
  | TEof
    deriving (Eq,Show)

data Lexeme = Lexeme
  { lexPos   :: !Position
  , lexToken :: Token
  } deriving Show

instance Eq Lexeme where
  a == b = lexToken a == lexToken b

data ErrorType
  = LexerError
  | ParserError
    deriving Show

data Error = Error ErrorType String Position deriving Show

data ParserState = ParserState
  { psInput   :: !L.Text
  , psChar    :: !Char
  , psPos     :: !Position
  , psLexCode :: !Int
  , psIndex   :: !Index
  } deriving Show

initParserState :: FilePath -> L.Text -> ParserState
initParserState path bs = ParserState
  { psInput   = bs
  , psChar    = '\n'
  , psPos     = initPosition path
  , psLexCode = 0
  , psIndex   = 0
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

-- | Raise an exception from the lexer.
raiseL :: String -> Parser a
raiseL msg = do
  st <- get
  raise (Error LexerError msg (psPos st))

-- | Raise an exception from the parser.
raiseP :: String -> Position -> Parser a
raiseP msg pos = raise (Error ParserError msg pos)

-- | Run the parser over the file given.
runParser :: FilePath -> L.Text -> Parser a -> Either Error a
runParser path bs m =
  case runM (unParser body) (initParserState path bs) of
    Right ((a,_),_) -> Right a
    Left err        -> Left err
  where
  body = do
    (res,errs) <- collect m
    unless (null errs)
        (raiseP ("definition errors: " ++ show errs) nullPosition)
    return res

-- | For testing parsers within ghci.
testParser :: Parser a -> String -> Either Error a
testParser p str = runParser "<interactive>" (L.pack str) p


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
  | DeclType (Forall Type)
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
mkTypeDecl :: Name -> Forall Type -> PDecls
mkTypeDecl n t = mempty { parsedPDecls = singleton n (DeclType t) }

-- | Quantify all free variables in a parsed type.
mkForall :: Type -> Forall Type
mkForall ty = quantify (Set.toList (typeVars ty')) ty'
  where
  ty' = numberTypeVars ty

-- | Syntactic numbering of type variables.
numberTypeVars :: Type -> Type
numberTypeVars ty = runST body
  where
  body = do
    ref <- newSTRef (0,Map.empty)
    loop ref ty

  loop ref (TApp l r)      = TApp      `fmap` loop ref l `ap` loop ref r
  loop ref (TInfix op l r) = TInfix op `fmap` loop ref l `ap` loop ref r
  loop _   t@TCon{}        = return t
  loop _   t@TGen{}        = return t
  loop ref (TVar p)        = do
    (n,m) <- readSTRef ref
    let var = paramName p
    case Map.lookup var m of
      Just ix -> return (TVar p { paramIndex = ix })
      Nothing -> do
        writeSTRef ref (n+1,Map.insert var n m)
        return (TVar p { paramIndex = n })


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

mkDataDecl :: DataDecl -> PDecls
mkDataDecl d = mempty { parsedDataDecls = [d] }

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
