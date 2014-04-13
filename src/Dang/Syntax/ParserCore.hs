{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Syntax.ParserCore where

import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Lexeme ( Lexeme )
import Dang.Utils.Location
import Dang.Utils.Pretty

import Control.Applicative ( Applicative(..) )
import Control.Monad ( MonadPlus(mzero) )
import Data.Maybe ( fromMaybe )
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
  } deriving (Functor,Applicative,Monad,MonadPlus)

instance BaseM Parser Dang where
  {-# INLINE inBase #-}
  inBase m = Parser (inBase m)

-- | Run the parser over the file given.
runParser :: [Lexeme] -> Parser a -> Dang a
runParser ls m = fst `fmap` runM (unParser m) (initParserState ls)


lexer :: (Lexeme -> Parser a) -> Parser a
lexer k =
  do ps <- Parser get
     case psTokens ps of

       l:ls -> do Parser (set ps { psTokens = ls })
                  k l

       [] -> fail "Unexpected end of input"

parseError :: Lexeme -> Parser a
parseError l =
  do addErrL (getLoc l) (text "Parser error near" <+> quoted (pp (unLoc l)))
     mzero


-- Helpers ---------------------------------------------------------------------

-- | Construct a Forall, using a type as the context.
mkForall :: Type -> Type -> Schema
mkForall cxt ty = Forall (mkProps cxt) ty

-- | Parse a context-kinded type into a list of props.
mkProps :: Type -> [Prop]
mkProps cxt = case cxt of
  TLoc lt    -> mkProps (unLoc lt)
  TTuple tys -> tys
  _          -> [cxt]

mkTuple :: [Type] -> Type
mkTuple tys = case tys of
  [ty] -> ty
  _    -> TTuple tys

mkTRow :: [Labelled Type] -> Maybe Type -> Type
mkTRow ls r = foldr TRowExt (fromMaybe TEmptyRow r) ls
