-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Dang.Syntax.Lexer (
    Token(..),
    Keyword(..),
    Lexeme(..),
    lexer,
    ignoreComments,
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Syntax.Location (Source)
import Dang.Utils.Ident

import           AlexTools
import           Data.Char (ord,isAscii,isSpace)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8)
import qualified Data.Text as T

}

$number      = [0-9]

$con_start   = [A-Z]
$ident_start = [a-z]
$middle      = [A-Za-z0-9_']

@con_name    = $con_start $middle*
@ident       = $ident_start $middle*
@qual        = (@con_name \. )+

:-

<0> {

$white+ ;

-- only single-line comments for now
"--" .*  { emits TLineComment }

-- keywords
"functor"{ keyword Kfunctor}
"sig"    { keyword Ksig    }
"struct" { keyword Kstruct }
"module" { keyword Kmodule }
"where"  { keyword Kwhere  }
"require"{ keyword Krequire}
"open"   { keyword Kopen   }
"forall" { keyword Kforall }
"type"   { keyword Ktype   }
"let"    { keyword Klet    }
"in"     { keyword Kin     }

-- punctuation
"|"      { keyword Kpipe   }
":"      { keyword Kcolon  }
"="      { keyword Kassign }
"("      { keyword Klparen }
")"      { keyword Krparen }
"->"     { keyword Krarrow }
"."      { keyword Kdot    }
","      { keyword Kcomma  }
"_"      { keyword Kwild   }

-- numbers
$number+ { emits (TNum 10 . read . T.unpack) }

-- names
@qual @con_name { emits (mkQual TQualCon) }
@con_name       { emits TUnqualCon        }

@qual @ident    { emits (mkQual TQualIdent) }
@ident          { emits TUnqualIdent        }

}

{

-- Tokens ----------------------------------------------------------------------

data Token = TUnqualCon !T.Text
           | TQualCon ![T.Text] !T.Text
           | TUnqualIdent !T.Text
           | TQualIdent ![T.Text] !T.Text
           | TKeyword !Keyword
           | TNum Int Integer
           | TLineComment !T.Text
           | TStart
           | TSep
           | TEnd
           | TError !T.Text -- ^ Lexical error
             deriving (Eq,Show)

isComment :: Token -> Bool
isComment TLineComment{} = True
isComment _              = False

ignoreComments :: [Lexeme Token] -> [Lexeme Token]
ignoreComments  = filter (isComment . lexemeToken)

mkQual :: ([T.Text] -> T.Text -> Token) -> T.Text -> Token
mkQual mk txt =
  case T.splitOn "." txt of
    [n] -> mk [] n
    []  -> error "impossible"
    ns  -> mk (init ns) (last ns)

data Keyword = Kmodule
             | Kfunctor
             | Ksig
             | Kstruct
             | Kwhere
             | Kcolon
             | Krequire
             | Kopen
             | Klparen
             | Krparen
             | Krarrow
             | Kassign
             | Ktype
             | Kforall
             | Kdot
             | Kcomma
             | Kwild
             | Kpipe
             | Klet
             | Kin
               deriving (Eq,Show)


-- Lexer -----------------------------------------------------------------------

mkConfig :: LexerConfig Mode Token
mkConfig  =
  LexerConfig { lexerInitialState = Normal
              , lexerStateMode    = modeToInt
              , lexerEOF          = \ _ -> [] }

lexer :: Source -> Maybe SourcePos -> T.Text -> [Lexeme Token]
lexer src mbPos bytes =
  $makeLexer mkConfig $
    case mbPos of
      Just pos -> (initialInput src bytes) { inputPos = pos }
      Nothing  ->  initialInput src bytes

emits :: (T.Text -> Token) -> Action Mode [Lexeme Token]
emits mkToken =
  do lexemeText  <- matchText
     lexemeRange <- matchRange
     return [Lexeme { lexemeToken = mkToken lexemeText, .. }]

token :: Token -> Action Mode [Lexeme Token]
token tok = emits (const tok)

keyword :: Keyword -> Action Mode [Lexeme Token]
keyword k = token (TKeyword k)


-- Lexer Modes -----------------------------------------------------------------

data Mode = Normal
            deriving (Show)

modeToInt :: Mode -> Int
modeToInt Normal = 0


-- Utility ---------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | isAscii c = fromIntegral (ord c)
  | otherwise = non_graphic

  where

  non_graphic = 0

alexGetByte = makeAlexGetByte $ \ c ->
  if isAscii c
     then toEnum (fromEnum c)
     else 0x1

}
