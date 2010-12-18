module Main where

import CodeGen
import Dang.IO
import Dang.Monad
import Error
import LambdaLift
import Pretty
import Rename
import Syntax.Parser
import Syntax.ParserCore
import qualified Syntax.AST as AST

import MonadLib
import Text.LLVM
import System.Environment (getArgs)
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main  = runDang $ do
  [file] <- inBase getArgs
  decls  <- loadSource file
  inBase . print . compile =<< lambdaLift (rename decls)

loadSource :: FilePath -> Dang [AST.Decl]
loadSource path = parseSource path =<< onFileNotFound (loadFile path) handler
  where
  handler x p = do
    inBase (putStrLn ("Unable to open file: " ++ p))
    raiseE x

parseSource :: FilePath -> UTF8.ByteString -> Dang [AST.Decl]
parseSource path source =
  case runParser path source parseFunBinds of
    Left err -> raiseDang (show err)
    Right ds -> return ds

rename :: [AST.Decl] -> [AST.Decl]
rename  = runLift . runRename [] . renameDecls

lambdaLift :: [AST.Decl] -> Dang [Decl]
lambdaLift ds = do
  (as,bs) <- runLL (llDecls ds)
  return (as ++ bs)

compile :: [Decl] -> Doc
compile ds = snd $ runLLVM $ do
  rtsImports
  compModule ds
