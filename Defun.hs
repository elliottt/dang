{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Defun where

import Error
import qualified AST

import Control.Applicative (Applicative(..))
import MonadLib


-- Defunctionalization Monad----------------------------------------------------

newtype Defun m a = Defun
  { unDefun :: m a
  } deriving (Functor,Applicative,Monad)


-- Defunctionalized Syntax -----------------------------------------------------

data Decl = Decl
  { declName     :: String
  , declExported :: Bool
  , declArity    :: Int
  , declBody     :: Term
  } deriving Show

data Call
  = CFun String
  | CArg Int
    deriving Show

data Term
  = Apply Call [Term]
  | Argument Int
  | Lit AST.Literal
    deriving Show

type Env = [(String,Call)]

defun :: [AST.Decl] -> [Decl]
defun ads = map (defunDecl env) ads
  where
  env = [ (n, CFun n) | ad <- ads, let n = AST.declName ad ]

defunDecl :: Env -> AST.Decl -> Decl
defunDecl env d = Decl
  { declName     = AST.declName d
  , declExported = AST.declExported d
  , declArity    = length vars
  , declBody     = defunTerm env vars (AST.declBody d)
  }
  where
  vars = AST.declVars d

defunTerm :: Env -> [AST.Var] -> AST.Term -> Term
defunTerm env args t =
  case t of
    AST.Abs vs t -> error "defunTerm: AST.Abs"
    AST.Let ds e -> undefined
    AST.App f xs -> undefined
    AST.Var v    -> undefined
    AST.Lit l    -> undefined
