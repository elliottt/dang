{-# LANGUAGE Safe #-}

module Dang.Colors (
    Command(), escape

  , setGraphics, withGraphics
  , Color()
  , black, red, green, yellow, blue, magenta, cyan, white
  , Mode()
  , fg, bg, bold, reset, underscore, blink
  ) where

import Data.List (intercalate)

newtype Color = Color { getColor :: Int }

black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = Color 0
red     = Color 1
green   = Color 2
yellow  = Color 3
blue    = Color 4
magenta = Color 5
cyan    = Color 6
white   = Color 7

newtype Mode = Mode { getMode :: Int }

fg :: Color -> Mode
fg c = Mode (30 + getColor c)

bg :: Color -> Mode
bg c = Mode(40 + getColor c)

reset, bold, underscore, blink :: Mode
reset      = Mode 0
bold       = Mode 1
underscore = Mode 4
blink      = Mode 5

escape :: Command -> String
escape cmd = "\ESC[" ++ getCommand cmd

newtype Command = Command { getCommand :: String }

setGraphics :: [Mode] -> Command
setGraphics ms = Command (intercalate ";" (map (show . getMode) ms) ++ "m")

withGraphics :: [Mode] -> String -> String
withGraphics ms msg =
  escape (setGraphics ms) ++ msg ++ escape (setGraphics [reset])
