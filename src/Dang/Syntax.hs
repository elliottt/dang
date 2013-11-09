{-# LANGUAGE Trustworthy #-}

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
import Dang.Monad ( Dang, io )
import Dang.Syntax.AST (Module)
import Dang.Syntax.Layout (layout)
import Dang.Syntax.Lexeme (Lexeme)
import Dang.Syntax.Lexer (scan)
import Dang.Syntax.Parser (parseModule)
import Dang.Syntax.ParserCore (runParser)
import Dang.Utils.Location (unLoc)
import Dang.Utils.Pretty (pretty)

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L


loadModule :: FilePath -> Dang Module
loadModule path = do
  logStage "parser"
  source <- loadFile path
  m      <- parseSource path source
  logInfo ("Parsed module\n" ++ pretty m)
  logDebug (show m)
  return m

parseSource :: FilePath -> L.Text -> Dang Module
parseSource path source =
  case runParser (layout (scan path source)) parseModule of
    Left err -> io (putStrLn (show err)) >> fail "Parse error"
    Right m  -> return m

testLexer :: FilePath -> String -> [Lexeme]
testLexer path = layout . scan path . L.pack

testLexer' :: FilePath -> IO ()
testLexer' path = do
  source <- L.readFile path
  mapM_ print (map unLoc (layout (scan path source)))

