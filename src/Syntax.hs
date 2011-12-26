module Syntax (
    loadModule
  , parseSource

  , Lexeme(..)
  , testLexer
  ) where

import Dang.IO (logStage,logInfo,logDebug,onFileNotFound,loadFile)
import Dang.Monad (Dang,io,raiseE)
import Pretty (pretty)
import Syntax.Lexeme (Lexeme(..))
import Syntax.Lexer (scan)
import Syntax.Parser (parseModule)
import Syntax.ParserCore (runParser)
import Syntax.AST (Module)

import qualified Data.Text.Lazy as L


loadModule :: FilePath -> Dang Module
loadModule path = do
  logStage "parser"
  source <- onFileNotFound (loadFile path) $ \ x p -> do
    io (putStrLn ("Unable to open file: " ++ p))
    raiseE x
  m <- parseSource path source
  logInfo "Parsed module"
  logDebug (show m)
  logInfo ('\n' : pretty m)
  return m

parseSource :: FilePath -> L.Text -> Dang Module
parseSource path source =
  case runParser (scan path source) parseModule of
    Left err -> io (putStrLn (show err)) >> fail "Parse error"
    Right m  -> return m

testLexer :: FilePath -> String -> [Lexeme]
testLexer path = scan path . L.pack
