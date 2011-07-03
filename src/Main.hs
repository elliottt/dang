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
import TypeChecker
import qualified Syntax.AST as AST

import MonadLib
import System.FilePath (dropExtension)
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main  = runDang $ do
  opts <- ask

  let a      = optAction opts
  logDebug ("Taking action: " ++ show a)
  let [file] = actionSources a

  m <- loadModule file
  logInfo "Parsed module"
  logDebug (show m)
  logInfo (pretty m)

  logDebug "Running module system"
  (iface,scm) <- scopeCheck m
  logDebug "Module system output"
  logDebug (show scm)

  logDebug "Kind checking"
  kcm <- kindCheckModule scm
  logDebug "Kind checking output:"
  logDebug (show kcm)

  compile iface kcm (ofile file)
  unless (optCompileOnly opts) (link [ofile file] (dropExtension file))

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

