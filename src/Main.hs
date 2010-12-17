module Main where

import CodeGen
import Dang.Monad
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
  source <- loadFile file
  decls  <- parseSource file source
  inBase . print . compile =<< lambdaLift (rename decls)

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
