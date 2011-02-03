module Main where

import Compile (compile)
import Dang.IO
import Dang.FileName
import Dang.Monad
import Link (link)
import ModuleSystem
import Pretty
import Syntax.Parser
import Syntax.ParserCore
import qualified Syntax.AST as AST

import MonadLib
import System.Exit (exitFailure)
import System.FilePath (dropExtension)
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main  = runDang $ do
  opts   <- ask
  file   <- oneSourceFile
  m      <- loadModule file
  logInfo "Parsed module"
  logDebug (show m)
  logInfo (pretty m)
  (iface,m') <- scopeCheck m
  logDebug "Module system output"
  logDebug (show m')
  compile iface m' (ofile file)
  unless (optCompileOnly opts) (link [ofile file] (dropExtension file))

oneSourceFile :: Dang FilePath
oneSourceFile  = do
  opts <- ask
  case optSourceFiles opts of
    [file] -> return file
    _      -> io (displayHelp [] >> exitFailure)


loadModule :: FilePath -> Dang AST.Module
loadModule path = parseSource path =<< onFileNotFound (loadFile path) handler
  where
  handler x p = do
    io (putStrLn ("Unable to open file: " ++ p))
    raiseE x

parseSource :: FilePath -> UTF8.ByteString -> Dang AST.Module
parseSource path source =
  case runParser path source parseModule of
    Left err -> io (putStrLn (show err)) >> fail "Parse error"
    Right m  -> return m

