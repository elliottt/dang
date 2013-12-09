{-# LANGUAGE PatternGuards #-}

module Dang.Syntax.Layout (
    layout
  , testLayout
  ) where

import Dang.Syntax.Lexer
import Dang.Syntax.Lexeme
import Dang.Utils.Location
import Dang.Utils.Panic
import Dang.Utils.Pretty hiding ( layout )

import qualified Data.Text.Lazy as L

import Debug.Trace


layout :: [Lexeme] -> [Lexeme]
layout  = normal []

testLayout :: String -> [Token]
testLayout str = map unLoc (layout (scan "" (L.pack str)))


-- | Layout scopes
data Scope = Explicit    -- ^ Layout scope started explicitly
           | Layout !Int -- ^ Implicit layout block
             deriving (Show,Eq)

layoutScope :: SrcLoc -> Scope
layoutScope src = Layout (posCol (locStart src))

lexCol :: Lexeme -> Int
lexCol l = posCol (locStart (getLoc l))


eof :: [Lexeme]
eof  = [TError "Unexpected end of input" `at` NoLoc]

type State = [Scope] -> [Lexeme] -> [Lexeme]

-- | Normal processing.
normal :: State
normal stack ts@(l:ls)
  | trace "normal" False = undefined

    -- close all scopes
  | TEof <- unLoc l = map (closeScope (getLoc l)) stack ++ [l]

    -- this token will begin a layout block
  | startsLayout (unLoc l) = l : startLayout stack ls

    -- end the current explicit block
  | TKeyword Krbrace  <- unLoc l
  , Explicit : stack' <- stack = l : normal stack' ls

    -- perform the offsides check
  | Layout n : stack' <- stack = offsides n stack' l stack ls

    -- emit the lexeme, and continue processing
  | otherwise = l : normal stack ls

  where
  -- close a single layout scope
  closeScope src scope = tok `at` src
    where
    tok = case scope of
      Layout _ -> TVirt VClose
      Explicit -> TError "missing `}`"

normal _ [] = eof


offsides :: Int -> [Scope] -> Lexeme -> State
offsides n stack' l stack ls
  | trace "offsides" False = undefined

    -- punctuate the current layout block
  | lexCol l == n = TVirt VSep `at` getLoc l : l : normal stack ls

    -- close the current layout block
  | lexCol l < n = TVirt VClose `at` getLoc l : l : normal stack' ls

    -- `in` will close a layout block
  | TKeyword Kin <- unLoc l = TVirt VClose `at` getLoc l : l : normal stack' ls

  | otherwise = l : normal stack ls

-- | Check if a token signals the start of a layout block
startsLayout :: Token -> Bool
startsLayout tok = case tok of
  TKeyword Kwhere   -> True
  TKeyword Klet     -> True
  TKeyword Kprivate -> True
  TKeyword Kpublic  -> True
  _                 -> False

-- | The next token begins layout.  Augment the stack, and transition back to
-- the normal state.
startLayout :: State
startLayout stack ts@(l:ls)
  | trace "startLayout" False = undefined

    -- an explicit block
  | TKeyword Klbrace <- unLoc l = l : normal (Explicit : stack) ls

    -- the column of this token begins the layout block
  | otherwise = TVirt VOpen `at` getLoc l
              : normal (layoutScope (getLoc l) : stack) ts

startLayout _ [] = eof
