{-# LANGUAGE PatternGuards #-}

module Dang.Syntax.Layout (
    layout
  , testLayout
  ) where

import Dang.Syntax.Lexer
import Dang.Syntax.Lexeme
import Dang.Utils.Location

import qualified Data.Text.Lazy as L


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
normal stack (l:ls)

    -- this token will begin a layout block
  | startsLayout (unLoc l) = l : startLayout stack ls

    -- end the current explicit block
  | TKeyword Krbrace  <- unLoc l
  , Explicit : stack' <- stack = l : normal stack' ls

    -- close all scopes
  | TEof <- unLoc l = map (closeScope (getLoc l)) stack ++ [l]

    -- perform the offsides check
  | Layout n : stack' <- stack = offsides l n stack' stack ls

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


-- | The passed token will be subject to the offsides rule, at the current
-- column.
offsides :: Lexeme -> Int -> [Scope] -> State
offsides l n stack' stack ls

    -- punctuate the current layout block, also detecting the start of a new
    -- layout block
  | lexCol l == n = virt VSep : l : if startsLayout (unLoc l) 
                                       then startLayout stack ls
                                       else normal stack ls

    -- close the current layout block, and pass the token back to the normal
    -- state, just in case it needs additional processing.  this will have the
    -- added bonus of transitioning back to the offsides state in the event that
    -- the token will close multiple levels of layout
  | lexCol l < n = virt VClose : normal stack' (l:ls)

    -- `in` will close a layout block
  | TKeyword Kin <- unLoc l = virt VClose : normal stack' (l:ls)

    -- the token doesn't require any special handling, emit it and continue as
    -- normal
  | otherwise = l : normal stack ls

  where

  virt t = TVirt t `at` getLoc l


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
startLayout stack (l:ls)

    -- an explicit block
  | TKeyword Klbrace <- unLoc l = l : normal (Explicit : stack) ls

    -- when the next token also begins a layout block, e.g.
    --
    -- module X where
    --  public
    --    x
    --
    -- where starts a layout block, then public starts a layout block
    -- immediately
  | startsLayout (unLoc l) = open : l : startLayout (virt : stack) ls

    -- the column of this token begins the layout block
  | otherwise = open : l : normal (virt : stack) ls

  where
  open = TVirt VOpen `at` getLoc l
  virt = layoutScope (getLoc l)

startLayout _ [] = eof
