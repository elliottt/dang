{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Dang.Syntax.Layout (
    layout
  ) where

import Dang.Syntax.Lexeme


-- Layout Levels ---------------------------------------------------------------

data Level = Level
  { levIndent :: !Int
  , levCont   :: Processor
  , levSep    :: Token
  }

-- | Generate a generally appropriate layout level.
posLevel :: Position -> Processor -> Token -> Level
posLevel p k tok = Level
  { levIndent = posCol p
  , levCont   = k
  , levSep    = tok
  }

semiLevel :: Position -> Processor -> Level
semiLevel p k = posLevel p k (TReserved ";")

pipeLevel :: Position -> Processor -> Level
pipeLevel p k = posLevel p k (TReserved "|")


-- Layout State ----------------------------------------------------------------

type State = [Level]

emptyState :: State
emptyState  = []

-- | Retrieve the processor for the current layout level.
continue :: State -> Layout
continue st = case st of
  l:_ -> levCont l st
  _   -> normal st

-- | Partition the layout levels by a dividing column.
splitLevels :: State -> Int -> ([Level],State)
splitLevels st col = span (\l -> col < levIndent l) st

-- | Remove as many levels as possible from the state, returning the number that
-- were closed.
closeLevels :: State -> Position -> (Int,State)
closeLevels st pos = case splitLevels st (posCol pos) of
  (cs,st') -> (length cs, st')

-- | Add a new level into the layout state.
pushLevel :: Level -> State -> Layout
pushLevel lev st = levCont lev (lev:st)

-- | Create a block starting token.
start :: Position -> Lexeme
start pos = Lexeme { lexPos = pos, lexToken = TReserved "{" }

-- | Emit a separator if a token falls on a level boundary.
sep :: State -> Lexeme -> [Lexeme]
sep st lx = case st of
  l:_ | levIndent l == posCol pos -> [Lexeme pos (levSep l),lx]
  _                               -> [lx]
  where
  pos = lexPos lx

-- | Create a block closing token.
close :: Position -> Lexeme
close pos = Lexeme { lexPos = pos, lexToken = TReserved "}" }


-- Lexeme Predicates -----------------------------------------------------------

-- | Lexemes that begin data-declaration layout.
startsData :: Lexeme -> Bool
startsData lx = case lexToken lx of
  TReserved "data" -> True
  _                -> False

-- | The ``='' token, when processed after a ``data'' token.
startsConstr :: Lexeme -> Bool
startsConstr lx = case lexToken lx of
  TReserved "=" -> True
  _             -> False

-- | Lexemes that begin normal layout.
startsBlock :: Lexeme -> Bool
startsBlock lx = case lexToken lx of
  TReserved "where"   -> True
  TReserved "let"     -> True
  TReserved "public"  -> True
  TReserved "private" -> True
  TReserved "of"      -> True
  _                   -> False

-- | Close levels, if there are levels to close.
closesBlock :: State -> Lexeme -> Maybe ([Lexeme],State)
closesBlock st lx = case closeLevels st pos of
  (n,st') | n <= 0    -> Nothing
          | otherwise -> Just (replicate n (close pos), st')
  where
  pos = lexPos lx


-- Layout Processing -----------------------------------------------------------

data Result
  = Done  [Lexeme] Layout
  | Delay [Lexeme] Layout

newtype Layout = Layout { stepLayout :: Lexeme -> Result }

type Processor = State -> Layout

-- | Process a stream of lexemes, adding in layout organizing lexemes.
layout :: [Lexeme] -> [Lexeme]
layout  = loop (normal emptyState)
  where
  loop l ls = case ls of
    lx:rest -> case stepLayout l lx of
      Done  out l' -> out ++ loop l' rest
      Delay out l' -> out ++ loop l' ls
    []      -> []

-- | Process tokens normally.
normal :: Processor
normal st = Layout go
  where
  go lx | startsData  lx                     = Done (sep st lx) (startData st)
        | startsBlock lx                     = Done (sep st lx) (startBlock st)
        | Just (cs,st') <- closesBlock st lx = Delay cs (continue st')
        | otherwise                          = Done (sep st lx) (normal st)

-- | Begin a layout block that is started by a data token.  This allows for
-- different processing of the ``='' token.
startData :: Processor
startData st = Layout go
  where
  go lx = Done [start pos, lx] (pushLevel (semiLevel pos dataGroups) st)
    where
    pos = lexPos lx

-- | Process a group of type declarations groups started by a ``='' token.
dataGroups :: Processor
dataGroups st = Layout go
  where
  go lx | startsConstr lx = Done (sep st lx) (startConstr st)
        | otherwise       = Done (sep st lx) (dataGroups st)

-- | Begin processing data constructors.
startConstr :: Processor
startConstr st = Layout go
  where
  go lx = Done [start pos,lx] (pushLevel (pipeLevel pos constr) st)
    where
    pos = lexPos lx

-- | Layout processing for data constructors.
constr :: Processor
constr st = Layout go
  where
  go lx | Just (cs,st') <- closesBlock st lx = Delay cs (continue st')
        | otherwise                          = Done (sep st lx) (constr st)

-- | Begin a layout level started by a normal layout token.
startBlock :: Processor
startBlock st = Layout go
  where
  go lx = Done [start pos,lx] (pushLevel (semiLevel pos next) st)
    where
    pos = lexPos lx
    next | startsBlock lx = startBlock
         | otherwise      = normal
