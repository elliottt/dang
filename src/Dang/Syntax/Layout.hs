{-# LANGUAGE RecordWildCards #-}

module Dang.Syntax.Layout (
    Layout(..),
    layout,
  ) where

import Dang.Syntax.Location


data Layout a = Layout { beginsLayout :: a -> Bool
                         -- ^ True when this token begins layout

                       , endsLayout :: a -> Bool
                         -- ^ True when this token explicitly ends layout

                       , sep :: a
                         -- ^ The separator token

                       , start :: a
                         -- ^ Layout block starting token

                       , end :: a
                         -- ^ Layout block ending token
                       }

layout :: Layout a -> [Located a] -> [Located a]
layout Layout { .. } = go Nothing []
  where
  startCol Range { rangeStart = Position { .. } } = posCol

  currentLevel (loc : _) = startCol loc
  currentLevel []        = 0

  close loc stack =
    let (closed,stack') = span (\loc' -> startCol loc < startCol loc') stack
     in (replicate (length closed) (end `at` loc), stack')

  go Just{} stack (tok@Located { .. } : toks) =
    (start `at` locRange) : tok : go Nothing (locRange:stack) toks

  go (Just loc) stack [] =
    (start `at` loc) : go Nothing (loc : stack) []

  go Nothing stack (tok@Located { .. } : toks)

    | beginsLayout locValue =
      tok : go (Just locRange) stack toks

    | endsLayout locValue || startCol locRange < currentLevel stack =
      let (closeToks,stack') = close locRange stack
       in closeToks ++ tok : go Nothing stack' toks

    | startCol locRange == currentLevel stack =
      (sep `at` locRange) : tok : go Nothing stack toks

    | otherwise =
      tok : go Nothing stack toks

  go _ stack [] =
    [ end `at` loc | loc <- stack ]
