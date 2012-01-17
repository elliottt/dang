{-# LANGUAGE TemplateHaskell #-}

module Syntax.Quote ( termQQ ) where

import Syntax.Parser (parseTerm)
import Syntax.ParserCore (runParserQ)

import Control.Monad ((<=<))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)


termQQ :: QuasiQuoter
termQQ  = QuasiQuoter
  { quoteExp  = lift <=< runParserQ parseTerm
  , quotePat  = fail "termQQ.quotePat"
  , quoteType = fail "ermQQ.quoteType"
  , quoteDec  = fail "ermQQ.quoteDec"
  }

