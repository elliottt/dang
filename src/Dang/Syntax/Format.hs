{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.Syntax.Format where

import Dang.Message (Message(..),MessageType(..),describeMessageType)
import Dang.Syntax.Lexer (lexer,Token(..),Keyword(..))
import Dang.Syntax.Location
           (Source(..),Located(..),Position(..),Range(..),zeroPos,rangeText
           ,rangeUnderline)
import Dang.Utils.PP

import           Control.Monad (forM_)
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L
import qualified System.Console.ANSI as Term


printMessage :: Source -> L.Text -> Message -> IO ()
printMessage src txt (Message ty loc doc) =
  do print (ppHeading (show (tyDoc <+> pp src <+> pp loc)))
     putStrLn ""
     print (describeMessageType ty)
     putStrLn ""
     gutterLen <- formatChunk src startPos (rangeText cxtLines loc txt)
     putStr (replicate gutterLen ' ')
     putStrLn (show (rangeUnderline loc))
     putStrLn ""
     print doc
  where
  cxtLines = 3

  tyDoc = case ty of
    Error{}   -> text "[error]"
    Warning{} -> text "[warning]"

  startRow = posRow (rangeStart loc) - fromIntegral cxtLines
  startPos = zeroPos { posRow = max 1 startRow }

  ppHeading msg =
    text "--" <+> text msg <+> text (replicate (80 - length msg - 4) '-')


-- | Print out a formatted chunk of source code to the console. The returned
-- value is the size of the line number gutter.
formatChunk :: Source -> Position -> L.Text -> IO Int
formatChunk src start chunk =
  do putStr (gutter (posRow start))
     spaces (posCol start - 1)
     go start toks

  where

  toks = lexer src (Just start) chunk

  pad = length (show (posRow (rangeEnd (locRange (last toks)))))

  gutter row =
    let str = show row
     in showString (replicate (pad - length str) ' ')
      $ showString str
      $ showChar '|' ""

  spaces n = putStr (replicate (fromIntegral n) ' ')

  newlines s e = forM_ [s .. e - 1] $ \ row ->
    do putStrLn ""
       putStr (gutter (row + 1))

  go pos (Located { locRange = Range { .. }, .. } : rest) =
    do if posRow rangeStart == posRow pos
          then spaces (posCol rangeStart - posCol pos)
          else do newlines (posRow pos) (posRow rangeStart)
                  spaces (posCol rangeStart - 1)
       formatToken locValue
       go rangeEnd rest

  go _ [] =
    do putStrLn ""
       return $! pad + 1


formatToken :: Token -> IO ()
formatToken (TUnqualCon c)    =    L.putStr c
formatToken (TQualCon ns n)   = do L.putStr (L.intercalate "." ns)
                                   putStr "."
                                   L.putStr n
formatToken (TUnqualIdent n)  =    L.putStr n
formatToken (TQualIdent ns n) = do L.putStr (L.intercalate "." ns)
                                   putStr "."
                                   L.putStr n
formatToken (TKeyword kw)     = formatKeyword kw
formatToken (TLineComment l)  = comment (L.putStr l)
-- XXX handle other bases
formatToken (TNum b i)        = foreground Term.Yellow (putStr (show i))

formatToken TStart            = return ()
formatToken TSep              = return ()
formatToken TEnd              = return ()
formatToken (TError s)        = L.putStr s


comment :: IO () -> IO ()
comment m =
  do Term.setSGR [Term.SetColor Term.Foreground Term.Dull Term.Green]
     m
     Term.setSGR []

foreground :: Term.Color -> IO () -> IO ()
foreground c m =
  do Term.setSGR [Term.SetColor Term.Foreground Term.Vivid c]
     m
     Term.setSGR []

formatKeyword :: Keyword -> IO ()
formatKeyword Kmodule  = foreground Term.Green   (putStr "module")
formatKeyword Kfunctor = foreground Term.Green   (putStr "functor")
formatKeyword Ksig     = foreground Term.Green   (putStr "sig")
formatKeyword Kstruct  = foreground Term.Green   (putStr "struct")
formatKeyword Kwhere   = foreground Term.Green   (putStr "where")
formatKeyword Kcolon   = foreground Term.Yellow  (putStr ":")
formatKeyword Kimport  = foreground Term.Magenta (putStr "import")
formatKeyword Kopen    = foreground Term.Green   (putStr "open")
formatKeyword Klparen  =                          putStr "("
formatKeyword Krparen  =                          putStr ")"
formatKeyword Krarrow  = foreground Term.Yellow  (putStr "->")
formatKeyword Kassign  = foreground Term.Yellow  (putStr "=")
formatKeyword Ktype    = foreground Term.Green   (putStr "type")
formatKeyword Kforall  = foreground Term.Green   (putStr "forall")
formatKeyword Kdot     = foreground Term.Yellow  (putStr ".")
formatKeyword Kcomma   =                          putStr ","
formatKeyword Kwild    =                          putStr "_"
formatKeyword Kpipe    = foreground Term.Yellow  (putStr "|")
formatKeyword Klet     = foreground Term.Yellow  (putStr "let")
formatKeyword Kin      = foreground Term.Yellow  (putStr "in")
