{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Dang.Syntax (
    -- * Parsing
    loadModule
  , parseSource

    -- * Testing
  , Lexeme
  , testLexer
  , testLexer'
  ) where

import Dang.IO (logStage,logInfo,logDebug,loadFile)
import Dang.Monad ( Dang, DangM, putErrs )
import Dang.Syntax.AST (Module)
import Dang.Syntax.Layout (layout)
import Dang.Syntax.Lexeme (Lexeme)
import Dang.Syntax.Lexer (scan)
import Dang.Syntax.Parser (parseModule)
import Dang.Syntax.ParserCore (runParser)
import Dang.Utils.Location (unLoc)
import Dang.Utils.Pretty (pretty)

import           Control.Monad ( mzero )
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           MonadLib ( BaseM(..) )


loadModule :: FilePath -> Dang Module
loadModule path = do
  logStage "parser"
  source <- loadFile path
  m      <- parseSource path source
  -- logInfo ("Parsed module\n" ++ pretty m)
  logDebug (show m)
  return m

parseSource :: BaseM dang Dang => FilePath -> L.Text -> dang Module
parseSource path source =
  inBase (runParser (layout (scan path source)) parseModule)

testLexer :: FilePath -> String -> [Lexeme]
testLexer path str = layout (scan path (L.pack str))

testLexer' :: FilePath -> IO ()
testLexer' path = do
  source <- L.readFile path
  mapM_ print (map unLoc (layout (scan path source)))

