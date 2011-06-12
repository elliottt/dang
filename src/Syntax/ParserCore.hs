{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax.ParserCore where

import QualName
import Syntax.AST
import TypeChecker.Types

import Control.Applicative (Applicative)
import Data.Either (partitionEithers)
import Data.Int (Int64)
import MonadLib
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map


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
  | TInt Int64
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
  { psInput   :: !S.ByteString
  , psChar    :: !Char
  , psPos     :: !Position
  , psLexCode :: !Int
  } deriving Show

initParserState :: FilePath -> S.ByteString -> ParserState
initParserState path bs = ParserState
  { psInput   = bs
  , psChar    = '\n'
  , psPos     = initPosition path
  , psLexCode = 0
  }

newtype Parser a = Parser
  { unParser :: StateT ParserState (ExceptionT Error Id) a
  } deriving (Functor,Applicative,Monad)

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser Error where
  raise = Parser . raise

instance RunExceptionM Parser Error where
  try m = Parser (try (unParser m))

-- | Raise an exception from the lexer.
raiseL :: String -> Parser a
raiseL msg = do
  st <- get
  raise (Error LexerError msg (psPos st))

-- | Raise an exception from the parser.
raiseP :: String -> Parser a
raiseP msg = do
  st <- get
  raise (Error ParserError msg (psPos st))

-- | Run the parser over the file given.
runParser :: FilePath -> S.ByteString -> Parser a -> Either Error a
runParser path bs (Parser m) =
  case runM m (initParserState path bs) of
    Right (a,_) -> Right a
    Left err    -> Left err

-- | For testing parsers within ghci.
testParser :: Parser a -> String -> Either Error a
testParser p str = runParser "<interactive>" (UTF8.fromString str) p


-- Parsed Syntax ---------------------------------------------------------------

type NameMap a = Map.Map Name (Clash a)

singleton :: Name -> a -> NameMap a
singleton n a = Map.singleton n (Ok a)

data Clash a = Ok a | Clash [a]
    deriving (Eq,Show,Ord)

-- | The elements of a clash.
clashElems :: Clash a -> [a]
clashElems (Ok a)     = [a]
clashElems (Clash as) = as

type Strategy a = a -> a -> Clash a

-- | Merge clashing values with a strategy for the initial clash.
mergeWithStrategy :: Strategy a -> Clash a -> Clash a -> Clash a
mergeWithStrategy strat (Ok a) (Ok b) = strat a b
mergeWithStrategy _     a      b      = Clash (clashElems a ++ clashElems b)

-- | Attempt to resolve clashes with a merge operation.
mergeNamedBy :: Strategy a -> NameMap a -> NameMap a -> NameMap a
mergeNamedBy strat = Map.unionWith (mergeWithStrategy strat)

-- | Add an element to a name map.
addNamed :: Strategy a -> Name -> a -> NameMap a -> NameMap a
addNamed strat n a = Map.insertWith (mergeWithStrategy strat) n (Ok a)

-- | Map over the elements of a name map.
mapNamed :: (a -> b) -> NameMap a -> NameMap b
mapNamed f = Map.map $ \ c -> case c of
  Ok a     -> Ok (f a)
  Clash as -> Clash (map f as)

-- | Always produce a clash when merging.
clash :: Strategy a
clash a b = Clash [a,b]

-- | Merge type and term declarations, when appropriate.
resolveTypes :: Strategy (Either (Forall Type) Decl)
resolveTypes (Right d) (Left t)  = Ok (Right (d { declType = Just t }))
resolveTypes (Left t)  (Right d) = Ok (Right (d { declType = Just t }))
resolveTypes a         b         = clash a b

-- | A collection of parsed declarations.  Loosely, this is a module.
data PDecls = PDecls
  { parsedDecls     :: NameMap (Either (Forall Type) Decl)
  , parsedOpens     :: [Open]
  , parsedPrimTerms :: NameMap PrimTerm
  , parsedPrimTypes :: NameMap PrimType
  } deriving (Show)

emptyPDecls :: PDecls
emptyPDecls  = PDecls
  { parsedDecls     = Map.empty
  , parsedOpens     = []
  , parsedPrimTerms = Map.empty
  , parsedPrimTypes = Map.empty
  }

mkDecl :: Decl -> PDecls
mkDecl d = emptyPDecls { parsedDecls = singleton (declName d) (Right d) }

mkDecls :: [Decl] -> PDecls
mkDecls ds = emptyPDecls { parsedDecls = foldl step Map.empty ds }
  where
  step m d = addNamed resolveTypes (declName d) (Right d) m

-- | Turn a block of declarations into a block of public declarations.
publicExport :: PDecls -> PDecls
publicExport pds = pds { parsedDecls = mapNamed step (parsedDecls pds) }
  where
  step (Right d) = Right d { declExport = Public }
  step e         = e

-- | Turn a block of declarations into a block of private declarations.
privateExport :: PDecls -> PDecls
privateExport pds = pds { parsedDecls = mapNamed step (parsedDecls pds) }
  where
  step (Right d) = Right d { declExport = Private }
  step e         = e

mkOpen :: Open -> PDecls
mkOpen o = emptyPDecls { parsedOpens = [o] }

mkPrimTerm :: PrimTerm -> PDecls
mkPrimTerm d = emptyPDecls { parsedPrimTerms = singleton (primTermName d) d }

mkPrimType :: PrimType -> PDecls
mkPrimType d = emptyPDecls { parsedPrimTypes = singleton (primTypeName d) d }

-- | Merge two sets of parsed declarations.
combinePDecls :: PDecls -> PDecls -> PDecls
combinePDecls ds1 ds2 = PDecls
  { parsedDecls     = merge resolveTypes parsedDecls
  , parsedOpens     = parsedOpens ds1 ++ parsedOpens ds2
  , parsedPrimTerms = merge clash parsedPrimTerms
  , parsedPrimTypes = merge clash parsedPrimTypes
  }
  where
  merge strat prj = mergeNamedBy strat (prj ds1) (prj ds2)

resolveNamed :: NameMap a -> ([a],[(Name,[a])])
resolveNamed  = Map.foldrWithKey step ([],[])
  where
  step _ (Ok a)     (as,bs) = (a:as,bs)
  step n (Clash es) (as,bs) = (as,(n,es):bs)

-- | Make a module from a set of parsed declarations, and a name.
mkModule :: QualName -> PDecls -> Parser Module
mkModule qn pds = do
  let (tds,derrs) = resolveNamed (parsedDecls pds)
      (tms,merrs) = resolveNamed (parsedPrimTerms pds)
      (tys,yerrs) = resolveNamed (parsedPrimTypes pds)
      (ts,ds)     = partitionEithers tds
  -- raise errors about derrs, merrs, yerrs and ts
  return Module
    { modName      = qn
    , modOpens     = parsedOpens pds
    , modDecls     = ds
    , modPrimTerms = tms
    , modPrimTypes = tys
    }
