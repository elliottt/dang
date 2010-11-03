module Main where

import Error
import LambdaLift
import Pretty
import Rename
import qualified AST

import MonadLib

type Base = ExceptionT SomeError IO

testRename :: [AST.Decl] -> IO ()
testRename ds = do
  e <- runExceptionT (runRename [] (renameDecls ds))
  case e of
    Left se -> print (se :: SomeError)
    Right a -> print a

testLL :: [AST.Decl] -> IO ()
testLL ds = do
  e <- runExceptionT (runLL . llDecls =<< runRename [] (renameDecls ds))
  case e of
    Left se -> print (se :: SomeError)
    Right a -> print a


idD    = AST.Decl "id" ["x"] True (AST.Var "x")
constD = AST.Decl "const" ["x", "y"] True (AST.Var "x")

test1 = [idD]

test2 =
  [ idD
  , constD
  , AST.Decl "test" [] True (AST.App (AST.Var "id") [AST.Var "const"])
  ]
