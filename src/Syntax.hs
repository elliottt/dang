module Syntax where

import Dang.IO (logStage,logInfo,logDebug,onFileNotFound,loadFile)
import Dang.Monad (Dang,io,raiseE)
import Pretty (pretty)
import Syntax.Parser (parseModule)
import Syntax.ParserCore (runParser)
import Syntax.AST (Module)

import qualified Data.ByteString.UTF8 as UTF8


loadModule :: FilePath -> Dang Module
loadModule path = do
  logStage "parser"
  source <- onFileNotFound (loadFile path) $ \ x p -> do
    io (putStrLn ("Unable to open file: " ++ p))
    raiseE x
  m <- parseSource path source
  logInfo "Parsed module"
  logDebug (show m)
  logInfo (pretty m)
  return m

parseSource :: FilePath -> UTF8.ByteString -> Dang Module
parseSource path source =
  case runParser path source parseModule of
    Left err -> io (putStrLn (show err)) >> fail "Parse error"
    Right m  -> return m

