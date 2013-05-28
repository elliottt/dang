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

fromSymIdent :: Token -> String
fromSymIdent tok = case tok of
  TSymIdent str -> str
  _             -> error "fromSymIdent: expected a TSymIdent"

fromConIdent :: Token -> String
fromConIdent tok = case tok of
  TConIdent str -> str
  _             -> error "fromConIdent: expected a TSymIdent"

fromOperIdent :: Token -> String
fromOperIdent tok = case tok of
  TOperIdent str -> str
  _              -> error "fromOperIdent: expected a TOperIdent"

fromInt :: Token -> Integer
fromInt tok = case tok of
  TInt str -> str
  _        -> error "fromInt: expected a TOperIdent"
