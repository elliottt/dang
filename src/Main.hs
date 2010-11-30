module Main where

import Compile
import Error
import LambdaLift
import Pretty
import Rename
import qualified AST

import MonadLib
import Text.LLVM

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
    Left se       -> print (se :: SomeError)
    Right (ds,ls) -> mapM_ print (map ppr (ds ++ ls))

testComp :: [AST.Decl] -> IO ()
testComp ds = do
  e <- runExceptionT (runLL . llDecls =<< runRename [] (renameDecls ds))
  case e of
    Left se        -> print (se :: SomeError)
    Right (ds',ls) -> print (snd (runLLVM (rtsImports >> compModule (ds'++ls))))


idD    = AST.Decl "id" ["x"] True (AST.Var "x")
constD = AST.Decl "const" ["x", "y"] True (AST.Var "x")

test1 = [idD]

test2 =
  [ idD
  , constD
  , AST.Decl "test" [] True (AST.App (AST.Var "id") [AST.Var "const"])
  ]

test3 =
  [ AST.Decl "test" [] True
    $ AST.App (AST.Var "id") [AST.Abs ["x"] (AST.Var "x")]
  , idD
  ]

test4 =
  [ AST.Decl "f" [] True
    $ AST.App (AST.Var "g") [AST.Var "x"]
  , AST.Decl "g" [] True
    $ AST.App (AST.Var "f") [AST.Var "y"]
  ]

test5 =
  [ idD
  , AST.Decl "_cvmain" [] True
    $ AST.App (AST.Var "id") [AST.Lit (AST.LInt 0)]
  ]

test6 =
  [ AST.Decl "_cvmain" [] True
    $ AST.Lit (AST.LInt 0)
  ]

main = return ()
