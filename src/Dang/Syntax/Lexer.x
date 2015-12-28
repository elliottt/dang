-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RecordWildCards #-}
module Dang.Syntax.Lexer (
    Token(..), Keyword(..), lexer
  ) where

import Dang.Syntax.AST (PName(..))
import Dang.Syntax.Location
import Dang.Utils.Ident

import           Data.Char (ord,isAscii)
import           Data.Word (Word8)
import qualified Data.Text.Lazy as L
}

$modStart   = [A-Z]
$identStart = [a-z]
$middle     = [A-Za-z0-9_']

:-

<0> {

$white+ ;

-- only single-line comments for now
"--" .* ;

-- keywords
"module" { keyword Kmodule }
"where"  { keyword Kwhere  }

-- names
$modStart $middle*   { emits (TModName . L.toStrict) }
$identStart $middle* { emits (TName    . PUnqual)    }

}

{

-- Tokens ----------------------------------------------------------------------

data Token = TModName !Namespace
           | TName !PName
           | TKeyword !Keyword
           | TError               -- ^ Lexical error
             deriving (Eq,Show)

data Keyword = Kmodule
             | Kwhere
             deriving (Eq,Show)


-- Actions ---------------------------------------------------------------------

type AlexAction = Maybe Source -> Int -> AlexInput -> Mode -> (Mode,[Located Token])

withInput :: (L.Text -> Token) -> Maybe Source -> Int -> AlexInput
          -> Located Token
withInput mk rangeSource len AlexInput { .. } =
  mk txt `at` Range { rangeStart = aiPos
                    , rangeEnd   = L.foldl' (flip movePos) aiPos txt
                    , .. }
  where
  txt = L.take (fromIntegral len) aiText

keyword :: Keyword -> AlexAction
keyword kw src len inp st = (st,[withInput (const (TKeyword kw)) src len inp])

emits :: (L.Text -> Token) -> AlexAction
emits mk src len inp st = (st,[withInput mk src len inp])


-- Lexer -----------------------------------------------------------------------

lexer :: Source
      -> L.Text
      -> [Located Token]
lexer src txt =
  go AlexInput { aiPos = Position 1 1 0, aiText = txt } Normal
  where

  go inp st =
    case alexScan inp (modeToInt st) of
      AlexEOF                -> []
      AlexError inp'         -> [TError `at` mkRange inp inp']
      AlexSkip inp' _        -> go inp' st
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
     return (byteForChar c, AlexInput { aiText = rest, .. })


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
