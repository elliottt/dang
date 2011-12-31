{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- vim: filetype=haskell
module Syntax.Lexer where

import Syntax.Lexeme

import Data.Int (Int64)
import MonadLib
import qualified Data.Text.Lazy as L
}

$digit       = [0-9]
$letter      = [a-zA-Z]
$lowerletter = [a-z]
$capletter   = [A-Z]
$symbol      = [\- \> \< \: \*]

@conident  = $capletter [$letter $digit [_ \! \? \']]*
@symident  = [_ $lowerletter] [$letter $digit [_ \! \? \']]*
@operident = $symbol+

:-

-- No nested comments, currently
<main,comment> {
"{-"            { begin comment }
}

<comment> {
"-}"            { begin 0 }
.               ;
}

<0> {

-- skip whitespace
$white          ;
"--".*$         ;

\\              { reserved }
"="             { reserved }
"("             { reserved }
")"             { reserved }
"let"           { reserved }
"in"            { reserved }
","             { reserved }
"."             { reserved }
"=>"            { reserved }
"|"             { reserved }

"data"          { reserved }
"module"        { reserved }
"where"         { reserved }
"open"          { reserved }
"as"            { reserved }
"hiding"        { reserved }
"public"        { reserved }
"private"       { reserved }
"forall"        { reserved }
"primitive"     { reserved }
"type"          { reserved }

@conident       { emitS TConIdent     }
@symident       { emitS TSymIdent     }
@operident      { emitS TOperIdent    }
$digit+         { emitS (TInt . read) }
}

{

newtype Lexer a = Lexer
  { unLexer :: StateT LexerState Id a
  } deriving (Functor,Monad)

instance StateM Lexer LexerState where
  get = Lexer   get
  set = Lexer . set

data LexerState = LexerState
  { lexerPosn  :: !Position
  , lexerChar  :: !Char
  , lexerInput :: !L.Text
  , lexerState :: !Int
  } deriving Show

scan :: FilePath -> L.Text -> [Lexeme]
scan source text = fst (runId (runStateT st0 (unLexer loop)))
  where
  st0  = LexerState
    { lexerPosn  = initPosition source
    , lexerInput = text
    , lexerChar  = '\n'
    , lexerState = 0
    }

  loop = do
    inp@(pos,_,_) <- alexGetInput
    sc            <- alexGetStartCode
    case alexScan inp sc of

      AlexToken inp' len action -> do
        alexSetInput inp'
        mb   <- action inp len
        rest <- loop
        case mb of
          Just lex -> return (lex:rest)
          Nothing  -> return rest

      AlexSkip inp' len -> do
        alexSetInput inp'
        loop

      AlexEOF ->
        return [Lexeme { lexPos = pos { posCol = 0 }, lexToken = TEof }]

      AlexError inp' -> return [Lexeme pos (TError "Lexical error")]


-- Input -----------------------------------------------------------------------

type AlexInput = (Position,Char,L.Text)

alexSetInput ::AlexInput -> Lexer ()
alexSetInput (pos,c,text) = do
  st <- get
  set $! st { lexerPosn = pos, lexerChar = c, lexerInput = text }

alexGetInput :: Lexer AlexInput
alexGetInput  = do
  st <- get
  return (lexerPosn st, lexerChar st, lexerInput st)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,bs) = do
  (c,bs') <- L.uncons bs
  return (c, (movePos p c, c, bs'))


-- Start Codes -----------------------------------------------------------------

alexGetStartCode :: Lexer Int
alexGetStartCode  = lexerState `fmap` get

alexSetStartCode :: Int -> Lexer ()
alexSetStartCode code = do
  s <- get
  set $! s { lexerState = code }


-- Actions ---------------------------------------------------------------------


type AlexAction result = AlexInput -> Int -> result

-- | Emit a token from the lexer
emitT :: Token -> AlexAction (Lexer (Maybe Lexeme))
emitT tok (pos,_,_) _ = return $ Just $! Lexeme
  { lexPos   = pos
  , lexToken = tok
  }

emitS :: (String -> Token) -> AlexAction (Lexer (Maybe Lexeme))
emitS mk (pos,c,bs) len = return $ Just $! Lexeme
  { lexPos   = pos
  , lexToken = mk (L.unpack (L.take (fromIntegral len) bs))
  }

reserved :: AlexAction (Lexer (Maybe Lexeme))
reserved  = emitS TReserved

begin :: Int -> AlexAction (Lexer (Maybe Lexeme))
begin code _ _ = alexSetStartCode code >> return Nothing

}

