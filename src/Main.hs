module Main where

import CodeGen
import Dang.IO
import Dang.FileName
import Dang.Monad
import Dang.Tool
import Interface
import LambdaLift
import ModuleSystem
import Pretty
import QualName
import ReadWrite
import Rename
import Syntax.Parser
import Syntax.ParserCore
import qualified Syntax.AST as AST

import MonadLib
import Text.LLVM
import System.IO (hPrint,hFlush)
import qualified Data.ByteString.UTF8 as UTF8

rtsPath :: FilePath
rtsPath  = "rts/librts.a"

main :: IO ()
main  = runDang $ do
  opts   <- ask
  let [file] = optSourceFiles opts
  m         <- loadModule file
  logDebug "Parsed module"
  logDebug (show m)
  (iface,m') <- scopeCheck m
  logDebug "Module system output"
  logDebug (show m')
  let m'' = rename m'
  logDebug "Renaming output"
  logDebug (show m'')
  decls     <- lambdaLift iface m''
  logDebug "Lambda-lifting output"
  logDebug (show decls)
  logDebug (unlines ["Lambda-lifted decls:", pretty decls])
  withOpenTempFile $ \ tmp h -> do
    asm <- compile (AST.modName m) iface decls
    io $ do
      hPrint h asm
      hFlush h
    sync llvm_as ["-o", ofile file, tmp ]
    unless (optCompileOnly opts)
      (sync llvm_ld ["-o", "prog", ofile file, rtsPath ])


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

rename :: AST.Module -> AST.Module
rename  = runLift . runRename [] . renameModule

lambdaLift :: Interface R -> AST.Module -> Dang [Decl]
lambdaLift iface m = do
  (as,bs) <- runLL iface (llModule m)
  return (as ++ bs)

compile :: QualName -> Interface R -> [Decl] -> Dang Doc
compile qn env ds = do
  writeInterface qn $! iface
  return doc
  where
  (iface,doc) = runLLVM (rtsImports >> compModule env ds)
