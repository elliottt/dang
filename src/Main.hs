module Main where

import CodeGen
import Dang.IO
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
  decls  <- loadModule file
  decls' <- lambdaLift (rename decls)
  inBase $ putStrLn $ unlines $ map (render . (char ';' <>) . ppr) decls'
  inBase $ print $ compile decls'

loadModule :: FilePath -> Dang AST.Module
loadModule path = parseSource path =<< onFileNotFound (loadFile path) handler
  where
  handler x p = do
    inBase (putStrLn ("Unable to open file: " ++ p))
    raiseE x

parseSource :: FilePath -> UTF8.ByteString -> Dang AST.Module
parseSource path source =
  case runParser path source parseModule of
    Left err -> inBase (putStrLn (show err)) >> fail "Parse error"
    Right m  -> return m

rename :: AST.Module -> AST.Module
rename  = runLift . runRename [] . renameModule

lambdaLift :: AST.Module -> Dang [Decl]
lambdaLift m = do
  (as,bs) <- runLL (llModule m)
  return (as ++ bs)

compile :: [Decl] -> Doc
compile ds = snd $ runLLVM $ do
  rtsImports
  compModule ds
