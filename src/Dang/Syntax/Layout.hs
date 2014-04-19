{-# LANGUAGE PatternGuards #-}

module Dang.Syntax.Layout (
    layout
  ) where

import Dang.Syntax.Lexeme
import Dang.Utils.Location


layout :: [Lexeme] -> [Lexeme]
layout  = normal False []


-- | Layout scopes
data Scope = Explicit Keyword -- ^ Layout scope started explicitly
           | Layout !Int      -- ^ Implicit layout block
           | Delimit Bool     -- ^ Delimited block marker.  The boolean
                              -- parameter indicates whether or not the
                              -- delimiter starts another block (local .. in ..,
                              -- vs let .. in ..)
             deriving (Show,Eq)

col :: SrcLoc -> Int
col src = posCol (locStart src)

-- | Check if a token signals the start of a layout block
startsLayout :: Token -> Bool
startsLayout tok = case tok of
  TKeyword Kwhere   -> True
  TKeyword Kprivate -> True
  TKeyword Kpublic  -> True
  TKeyword Krec     -> True
  TKeyword Kof      -> True
  TKeyword Klambda  -> True
  _                 -> False

eof :: Lexeme
eof  = TError "Unexpected end of input" `at` NoLoc

normal :: Bool -> [Scope] -> [Lexeme] -> [Lexeme]
normal start stack (l@(Located src t):ls)
  | startsLayout        t = toks ++ normal True stack' ls
  | TKeyword Klet    == t = toks ++ normal True (Delimit False : stack') ls
  | TKeyword Klocal  == t = toks ++ normal True (Delimit True  : stack') ls
  | TKeyword Klbrace == t = toks ++ normal False (Explicit Krbrace:stack') ls
  | TKeyword Klparen == t = toks ++ normal False (Explicit Krparen:stack') ls
  | TKeyword Kin     == t = toks ++ normal dstart stack' ls
  | TEof             == t = toks
  | otherwise             = toks ++ normal False stack' ls
  where

  -- such value recursion
  (dstart,toks,offStack) = offsides vs l stack
  (vs,stack')
    | start && TKeyword Klbrace == t = ([], Explicit Krbrace : offStack)

    | start = ([TVirt Vopen `at` src], Layout (col src) : offStack)

    | otherwise = ( [], offStack )

normal _ _ _ = [eof]


-- Offsides Check --------------------------------------------------------------

-- | Perform the offsides check, given context.
offsides :: [Lexeme] -> Lexeme -> [Scope] -> (Bool,[Lexeme],[Scope])
offsides vs l@(Located src t) = go vs
  where
  go virts stack = case stack of

    -- close a delimited block with `in`
    Delimit b : rest | TKeyword Kin == t -> delimit b virts rest

    -- can't close delimited blocks that are explicitly open
    Explicit _ : rest | TKeyword Kin == t -> done (lexErr : virts) rest

    -- close virtual blocks up to the start of a delimited block
    Layout _ : rest | TKeyword Kin == t -> go (virt Vclose : virts) rest

    -- separate or close the current layout block
    Layout c : rest
      | col src == c -> done (virt Vsep : virts) stack
      | col src < c  -> go (virt Vclose : virts) rest

    -- close layout blocks while this is a closing token
    Layout _ : rest
      | closingToken -> go (virt Vclose : virts) rest

    -- close an explicit block when it's the expected closing token
    Explicit close : rest | TKeyword close == t -> done virts rest

    _ -> done virts stack

  delimit b ts s = (b, reverse (l:ts), s)
  done      ts s = (False, reverse (l:ts), s)

  virt v = TVirt v `at` src

  lexErr = TError "Lexical error" `at` src

  closingToken = t == TKeyword Krbrace
              || t == TKeyword Krparen
