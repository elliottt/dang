module Syntax.Lexeme where


-- Positions -------------------------------------------------------------------

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


-- Tokens ----------------------------------------------------------------------

data Token
  = TReserved String
  | TConIdent String
  | TSymIdent String
  | TOperIdent String
  | TInt Integer
  | TEof
  | TError String
    deriving (Eq,Show)

isEof :: Token -> Bool
isEof TEof = True
isEof _    = False


-- Lexemes ---------------------------------------------------------------------

data Lexeme = Lexeme
  { lexPos   :: !Position
  , lexToken :: Token
  } deriving Show

instance Eq Lexeme where
  a == b = lexToken a == lexToken b
