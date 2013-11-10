{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Syntax.ParserCore where

import Dang.Monad ( Dang, Error(..) )
import Dang.Syntax.Lexeme ( Lexeme(..) )
import Dang.Utils.Location ( SrcLoc, Located )
import Dang.Utils.Pretty

import Control.Applicative ( Applicative(..) )
import Control.Monad ( unless )
import MonadLib ( BaseM(..), runM, StateT, get, set )


-- Lexer/Parser Monad ----------------------------------------------------------

data ParserState = ParserState { psTokens  :: [Lexeme]
                               } deriving Show

initParserState :: [Lexeme] -> ParserState
initParserState ls =
  ParserState { psTokens  = ls
              }

newtype Parser a = Parser
  { unParser :: StateT ParserState Dang a
  } deriving (Functor,Applicative,Monad)

instance BaseM Parser Dang where
  {-# INLINE inBase #-}
  inBase m = Parser (inBase m)

-- | Run the parser over the file given.
runParser :: [Lexeme] -> Parser a -> Dang a
runParser ls m = fst `fmap` runM (unParser m) (initParserState ls)
