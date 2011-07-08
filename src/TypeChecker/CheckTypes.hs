module TypeChecker.CheckTypes where

import QualName
import Syntax.AST
import TypeChecker.Monad
import TypeChecker.Types

import MonadLib
import qualified Data.Foldable as F


-- Type Checking ---------------------------------------------------------------

tcTerm :: Term -> TC (Type,Term)
tcTerm tm = case tm of

  Abs vs b -> undefined

  Let ds e -> undefined

  App f xs -> undefined

  Local n -> undefined

  Global qn -> undefined

  Lit lit -> undefined

  Prim v -> undefined
