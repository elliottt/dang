module Main where

import CodeGen
import Error
import LambdaLift
import Pretty
import Rename
import Syntax.Parser
import Syntax.ParserCore
import qualified Syntax.AST        as AST

import MonadLib
import Text.LLVM
import qualified Data.ByteString.UTF8 as UTF8

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
    Left se        -> print (se :: SomeError)
    Right (ds',ls) -> mapM_ print (map ppr (ds' ++ ls))

testComp :: [AST.Decl] -> IO ()
testComp ds = do
  e <- runExceptionT (runLL . llDecls =<< runRename [] (renameDecls ds))
  case e of
    Left se        -> print (se :: SomeError)
    Right (ds',ls) -> print $ snd $ runLLVM $ do
      rtsImports
      compModule (ds' ++ ls)

testParse :: String -> IO ()
testParse str =
  case runParser "<interactive>" (UTF8.fromString str) parseFunBinds of
    Left err -> print err
    Right a  -> putStrLn (pretty a)


idD :: AST.Decl
idD  = AST.Decl "id" ["x"] True (AST.Var "x")

constD :: AST.Decl
constD  = AST.Decl "const" ["x", "y"] True (AST.Var "x")

oneD :: AST.Decl
oneD  = AST.Decl "one" [] False 1

type Test = [AST.Decl]

test1 :: Test
test1  = [idD]

test2 :: Test
test2  =
  [ idD
  , constD
  , AST.Decl "test" [] True (AST.App (AST.Var "id") [AST.Var "const"])
  ]

test3 :: Test
test3  =
  [ AST.Decl "test" [] True
    $ AST.App (AST.Var "id") [AST.Abs ["x"] (AST.Var "x")]
  , idD
  ]

test4 :: Test
test4  =
  [ AST.Decl "f" [] True
    $ AST.App (AST.Var "g") [AST.Var "x"]
  , AST.Decl "g" [] True
    $ AST.App (AST.Var "f") [AST.Var "y"]
  ]

test5 :: Test
test5  =
  [ idD
  , AST.Decl "_cvmain" [] True
    $ AST.App (AST.Var "id") [AST.Lit (AST.LInt 0)]
  ]

test6 :: Test
test6  =
  [ AST.Decl "_cvmain" [] True
    $ AST.Lit (AST.LInt 0)
  ]

test7 :: Test
test7  =
  [ AST.Decl "_cvmain" [] True (abs (1 + (negate (1 * 3))))
  ]

test8 :: Test
test8  =
  [ AST.Decl "_cvmain" []    True (AST.App (AST.Var "f") [AST.Var "id"])
  , AST.Decl "f"       ["g"] True (AST.App (AST.Var "g") [0])
  , idD
  ]

test9 :: Test
test9  =
  [ AST.Decl "_cvmain" [] True
    $ AST.Let [idD,oneD] $ AST.apply (AST.Var "id") [AST.Var "one"]
  ]

main :: IO ()
main  = return ()
