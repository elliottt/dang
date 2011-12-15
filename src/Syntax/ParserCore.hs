{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax.ParserCore where

import Data.ClashMap as CM
import ModuleSystem.Export (Export(..))
import QualName
import Syntax.AST
import Syntax.Renumber (renumber)
import TypeChecker.Types
import TypeChecker.Unify

import Control.Applicative (Applicative)
import Data.Monoid (Monoid(..))
import MonadLib
import qualified Data.Text.Lazy as L
import qualified Data.Set as Set
import qualified Data.Sequence as Seq


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

isEof :: Token -> Bool
isEof TEof = True
isEof _    = False

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

data Level = Level
  { levBlock  :: Bool
  , levIndent :: !Int
  } deriving Show

zeroLevel :: Level
zeroLevel  = Level { levBlock = False, levIndent = 0 }

data ParserState = ParserState
  { psInput   :: !L.Text
  , psChar    :: !Char
  , psPos     :: !Position
  , psLexCode :: !Int
  , psIndex   :: !Index
  , psLevels  :: [Level]
  , psBegin   :: Bool
  , psBlock   :: Bool
  , psDelay   :: Seq.Seq Lexeme
  } deriving Show

initParserState :: FilePath -> L.Text -> ParserState
initParserState path bs = ParserState
  { psInput   = bs
  , psChar    = '\n'
  , psPos     = initPosition path
  , psLexCode = 0
  , psIndex   = 0
  , psLevels  = [zeroLevel]
  , psBegin   = False
  , psBlock   = False
  , psDelay   = Seq.empty
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

testFile :: Parser a -> FilePath -> IO (Either Error a)
testFile p file = testParser p `fmap` readFile file

-- | For testing parsers within ghci.
testParser :: Parser a -> String -> Either Error a
testParser p str = runParser "<interactive>" (L.pack str) p


-- Layout Processing -----------------------------------------------------------

layout :: Parser Lexeme -> Parser Lexeme
layout scan = loop
  where
  open  pos = Lexeme { lexPos = pos, lexToken = TReserved "{" }
  sep   pos = Lexeme { lexPos = pos, lexToken = TReserved ";" }
  close pos = Lexeme { lexPos = pos, lexToken = TReserved "}" }

  loop = checkDelayed >>= \mb -> case mb of

    -- nothing delayed, proceed.
    Nothing -> do
      l <- scan
      let tok = lexToken l
          pos = lexPos l

      -- act on a new layout level
      level <- getLevel
      ps    <- get
      l'    <- case psBegin ps of

        _res | isEof tok -> do
          ls <- getLevels
          let c = close pos
          -- we never close the last level, and we close the current one, so 2.
          replicateM_ (length ls - 2) (delayLexeme c)
          delayLexeme l
          return c

        False -> case compare (posCol pos) (levIndent level) of
          EQ -> delayLexeme l >> return (sep pos)
          GT -> return l
          -- this needs to pop blocks until it hits one at the correct level.
          LT -> do
            popLevel
            when (levBlock level) (delayLexeme (sep pos))
            delayLexeme l
            return (close pos)

        True -> do
          pushLevel (posCol pos)
          clearBegin
          delayLexeme l
          return (open pos)

      -- schedule a new layout level
      when (beginsLayout tok) (nextBeginsLayout tok)

      return l'

    -- not common that a token gets delayed by a layout token
    Just l -> return l

checkDelayed :: Parser (Maybe Lexeme)
checkDelayed  = do
  ps <- get
  case Seq.viewl (psDelay ps) of
    Seq.EmptyL  -> return Nothing
    l Seq.:< ls -> do
      set ps { psDelay = ls }
      return (Just l)

delayLexeme :: Lexeme -> Parser ()
delayLexeme l = do
  ps <- get
  set ps { psDelay = psDelay ps Seq.|> l }

beginsLayout :: Token -> Bool
beginsLayout tok = case tok of
  TReserved "let"     -> True
  TReserved "do"      -> True
  TReserved "where"   -> True
  TReserved "public"  -> True
  TReserved "private" -> True
  TReserved "data"    -> True
  _                   -> False

opensBlock :: Token -> Bool
opensBlock tok = case tok of
  TReserved "public"  -> True
  TReserved "private" -> True
  TReserved "where"   -> True
  _                   -> False

-- | Signal that the next token begins a level of indentation.
nextBeginsLayout :: Token -> Parser ()
nextBeginsLayout tok = do
  ps <- get
  set ps { psBegin = True, psBlock = opensBlock tok }

clearBegin :: Parser ()
clearBegin  = do
  ps <- get
  set ps { psBegin = False, psBlock = False }

getLevels :: Parser [Level]
getLevels  = psLevels `fmap` get

getLevel :: Parser Level
getLevel  = do
  ls <- getLevels
  case ls of
    l:_ -> return l
    []  -> fail "Syntax.ParserCore.getLevel: the unexpected happened"

pushLevel :: Int -> Parser ()
pushLevel l = do
  ps <- get
  let lev = Level { levIndent = l, levBlock = psBlock ps }
  set $! ps { psLevels = lev : psLevels ps }

popLevel :: Parser ()
popLevel  = do
  ps <- get
  case psLevels ps of
    _:ls -> set ps { psLevels = ls }
    []   -> fail "Syntax.ParserCore.popLevel: the unexpected happened"


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
  ty' = renumber ty

mkConstrGroup :: Name -> [Type] -> [Constr] -> Forall ConstrGroup
mkConstrGroup n tys cs = quantify vars ConstrGroup
  { groupType    = ty
  , groupConstrs = renumber cs
  }
  where
  tys' = renumber tys
  ty   = foldl tarrow (TCon (simpleName n)) tys'
  vars = Set.toList (typeVars tys')


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
