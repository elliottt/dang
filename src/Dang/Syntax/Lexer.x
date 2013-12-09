-- vim: filetype=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Syntax.Lexer where

import Dang.Syntax.Lexeme
import Dang.Utils.Location

import Data.Bits (shiftR,(.&.))
import Data.Int (Int64)
import Data.Monoid (mempty)
import Data.Word (Word8)
import MonadLib
import qualified Data.Text.Lazy as L
}

$digit       = [0-9]
$letter      = [a-zA-Z]
$lowerletter = [a-z]
$capletter   = [A-Z]
$symbol      = [\- \> \< \: \*]

@conident  = $capletter [$letter $digit [_ \! \? \']]*
@ident     = [_ $lowerletter] [$letter $digit [_ \! \? \']]*
@operident = $symbol+

:-

-- No nested comments, currently
<comment> {
"-}"            { begin 0 }
.               ;
}

<0> {

-- skip whitespace
$white          ;
"--".*$         ;
"{-"            { begin comment }

-- reserved symbols
\\              { keyword Klambda     }
"="             { keyword Kassign     }
"("             { keyword Klparen     }
")"             { keyword Krparen     }
"["             { keyword Klbracket   }
"]"             { keyword Krbrace     }
"{"             { keyword Klbrace     }
"}"             { keyword Krbrace     }
","             { keyword Kcomma      }
"."             { keyword Kdot        }
"=>"            { keyword KfatArrow   }
"|"             { keyword Kpipe       }
"_"             { keyword Kunderscore }

-- keywords
"let"           { keyword Klet       }
"in"            { keyword Kin        }
"data"          { keyword Kdata      }
"module"        { keyword Kmodule    }
"where"         { keyword Kwhere     }
"open"          { keyword Kopen      }
"as"            { keyword Kas        }
"hiding"        { keyword Khiding    }
"public"        { keyword Kpublic    }
"private"       { keyword Kprivate   }
"forall"        { keyword Kforall    }
"primitive"     { keyword Kprimitive }
"type"          { keyword Ktype      }
"case"          { keyword Kcase      }
"of"            { keyword Kof        }

@conident       { emitS TConIdent     }
@ident          { emitS TIdent        }
@operident      { emitS TOperIdent    }
$digit+         { emitS ((`TInt` 10) . read) }
}

{

-- Input Operations ------------------------------------------------------------

type AlexInput = LexerInput

data LexerInput = LexerInput
  { liPosn   :: !Position
  , liSource :: FilePath
  , liChar   :: !Char
  , liBytes  :: [Word8]
  , liInput  :: L.Text
  } deriving (Show)

initLexerInput :: FilePath -> L.Text -> LexerInput
initLexerInput source bytes = LexerInput
  { liPosn   = Position 0 1 1
  , liSource = source
  , liChar   = '\n'
  , liBytes  = []
  , liInput  = bytes
  }

-- | Build a range from the lexer state.
mkRange :: LexerInput -> String -> SrcLoc
mkRange li str =
  SrcLoc (Range (liPosn li) (movesPos (liPosn li) str)) (Just (liSource li))

fillBuffer :: LexerInput -> Maybe LexerInput
fillBuffer li = do
  (c,rest) <- L.uncons (liInput li)
  return $! li
    { liPosn  = movePos (liPosn li) c
    , liBytes = utf8Encode c
    , liChar  = c
    , liInput = rest
    }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar  = liChar

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte li = case liBytes li of
  b:bs -> return (b, li { liBytes = bs })
  _    -> alexGetByte =<< fillBuffer li

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 +  oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + ( oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 +   oc             .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + ( oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6)  .&. 0x3f)
                        , 0x80 +   oc              .&. 0x3f
                        ]


-- Lexer Monad -----------------------------------------------------------------

newtype Lexer a = Lexer
  { unLexer :: StateT LexerState Id a
  } deriving (Functor,Monad)

instance StateM Lexer LexerState where
  get = Lexer   get
  set = Lexer . set

data LexerState = LexerState
  { lexerInput :: !LexerInput
  , lexerState :: !Int
  } deriving Show

scan :: FilePath -> L.Text -> [Lexeme]
scan source bytes = fst (runId (runStateT st0 (unLexer loop)))
  where
  st0  = LexerState
    { lexerInput = initLexerInput source bytes
    , lexerState = 0
    }

  loop = do
    inp <- alexGetInput
    sc  <- alexGetStartCode
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
        return [Located mempty TEof]

      AlexError inp' ->
        return [Located (mkRange inp' "") (TError "Lexical error")]

alexSetInput :: AlexInput -> Lexer ()
alexSetInput inp = do
  st <- get
  set $! st { lexerInput = inp }

alexGetInput :: Lexer AlexInput
alexGetInput  = lexerInput `fmap` get


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
emitT tok = emitS (const tok)

emitS :: (String -> Token) -> AlexAction (Lexer (Maybe Lexeme))
emitS mk li len = return (Just $! Located (mkRange li str) (mk str))
  where
  range = mkRange li str
  str   = L.unpack (L.take (fromIntegral len) (liInput li))

keyword :: Keyword -> AlexAction (Lexer (Maybe Lexeme))
keyword kw = emitT (TKeyword kw)

begin :: Int -> AlexAction (Lexer (Maybe Lexeme))
begin code _ _ = alexSetStartCode code >> return Nothing

}

