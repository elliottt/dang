{-# LANGUAGE Safe #-}

module Dang.Syntax.Lexeme where

import Dang.Utils.Location


-- Positions -------------------------------------------------------------------

type Lexeme = Located Token

data Token
  = TReserved String
  | TConIdent String
  | TSymIdent String
  | TOperIdent String
  | TInt Integer
  | TEof
  | TError String
    deriving (Eq,Show)

isEof :: Token -> Bool
isEof TEof = True
isEof _    = False
