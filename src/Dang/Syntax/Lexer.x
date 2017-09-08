-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Dang.Syntax.Lexer (
    Token(..), Keyword(..), lexer, ignoreComments
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Syntax.Location
import Dang.Utils.Ident

import           Data.Char (ord,isAscii,isSpace)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8)
import qualified Data.Text.Lazy as L

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
$number+ { emits (TNum 10 . read . L.unpack) }

-- names
@qual @con_name { emits (mkQual TQualCon) }
@con_name       { emits TUnqualCon        }

@qual @ident    { emits (mkQual TQualIdent) }
@ident          { emits TUnqualIdent        }

}

{

-- Tokens ----------------------------------------------------------------------

data Token = TUnqualCon !L.Text
           | TQualCon ![L.Text] !L.Text
           | TUnqualIdent !L.Text
           | TQualIdent ![L.Text] !L.Text
           | TKeyword !Keyword
           | TNum Integer Int
           | TLineComment !L.Text
           | TStart
           | TSep
           | TEnd
           | TError !L.Text -- ^ Lexical error
             deriving (Eq,Show)

isComment :: Token -> Bool
isComment TLineComment{} = True
isComment _              = False

mkQual :: ([L.Text] -> L.Text -> Token) -> L.Text -> Token
mkQual mk txt =
  case L.splitOn "." txt of
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


-- Actions ---------------------------------------------------------------------

type AlexAction = Maybe Source -> Int -> AlexInput -> Mode -> (Mode,[SrcLoc Token])

move :: Char -> Position -> Position
move  = movePos 8

withInput :: (L.Text -> Token) -> Maybe Source -> Int -> AlexInput
          -> SrcLoc Token
withInput mk rangeSource len AlexInput { .. } =
  mk txt `at` Range { rangeStart = aiPos
                    , rangeEnd   = L.foldl' (flip move) aiPos txt
                    , .. }
  where
  txt = L.take (fromIntegral len) aiText

keyword :: Keyword -> AlexAction
keyword kw src len inp st = (st,[withInput (const (TKeyword kw)) src len inp])

emits :: (L.Text -> Token) -> AlexAction
emits mk src len inp st = (st,[withInput mk src len inp])


-- Lexer -----------------------------------------------------------------------

ignoreComments :: [SrcLoc Token] -> [SrcLoc Token]
ignoreComments  = filter (not . isComment . thing)

lexer :: Source
      -> Maybe Position
      -> L.Text
      -> [SrcLoc Token]
lexer src mbPos txt =
  go AlexInput { aiPos = startPos, aiText = txt } Normal
  where

  startPos = fromMaybe (Position 1 1) mbPos

  go inp st =
    case alexScan inp (modeToInt st) of

      AlexEOF ->
        []

      -- chew up text until the next whitespace character, then continue
      AlexError inp' ->
        let (as,bs) = L.break isSpace (aiText inp')
            pos'    = L.foldl' (flip move) (aiPos inp') as
            inp2    = AlexInput { aiPos = pos', aiText = bs }
            loc     = Range { rangeStart = aiPos inp', rangeEnd = pos', .. }
        in (TError as `at` loc) : go inp2 st

      AlexSkip inp' _ ->
        go inp' st

      AlexToken inp' len act ->
        case act rangeSource len inp st of
          (st',xs) -> xs ++ go inp' st'

  rangeSource = Just src

  mkRange a b =
    Range { rangeStart = aiPos a, rangeEnd = aiPos b, .. }


-- Alex Interface --------------------------------------------------------------

data AlexInput = AlexInput { aiPos  :: !Position
                           , aiText :: L.Text
                           }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte AlexInput { .. } =
  do (c,rest) <- L.uncons aiText
     return (byteForChar c, AlexInput { aiText = rest, aiPos = move c aiPos, .. })


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


}
