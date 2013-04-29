{-# LANGUAGE QuasiQuotes #-}

module Dang.TypeChecker.Quote where

import Dang.Syntax.Parser (parseType,parseScheme)
import Dang.Syntax.ParserCore (runParserQ)

import Control.Monad ((<=<))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

typeQQ :: QuasiQuoter
typeQQ  = QuasiQuoter
  { quoteExp  = lift <=< runParserQ parseType
  , quotePat  = fail "typeQQ.quotePat"
  , quoteType = fail "typeQQ.quoteType"
  , quoteDec  = fail "typeQQ.quoteDec"
  }

schemeQQ :: QuasiQuoter
schemeQQ  = QuasiQuoter
  { quoteExp  = lift <=< runParserQ parseScheme
  , quotePat  = fail "schemeQQ.quotePat"
  , quoteType = fail "schemeQQ.quoteType"
  , quoteDec  = fail "schemeQQ.quoteDec"
  }

