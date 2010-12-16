module Main where

import CodeGen
import LambdaLift
import Pretty
import Rename
import Syntax.Parser
import Syntax.ParserCore
import qualified Syntax.AST as AST

import MonadLib
import Text.LLVM
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.ByteString      as S
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main  = do
  [file] <- getArgs
  source <- S.readFile file
  decls  <- parseSource file source
  print . compile =<< lambdaLift (rename decls)

parseSource :: FilePath -> UTF8.ByteString -> IO [AST.Decl]
parseSource path source =
  case runParser path source parseFunBinds of
    Left err -> print err >> exitFailure
    Right ds -> return ds

rename :: [AST.Decl] -> [AST.Decl]
rename  = runLift . runRename [] . renameDecls

lambdaLift :: [AST.Decl] -> IO [Decl]
lambdaLift ds = do
  e <- runExceptionT (runLL (llDecls ds))
  case e of
    Left se       -> print se >> exitFailure
    Right (as,bs) -> return (as ++ bs)

compile :: [Decl] -> Doc
compile ds = snd $ runLLVM $ do
  rtsImports
  compModule ds
