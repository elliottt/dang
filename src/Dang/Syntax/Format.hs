{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.Syntax.Format where

import Dang.Syntax.Lexer (Token(..),Keyword(..))
import Dang.Syntax.Location
           (Source(..),Located(..),Position(..),zeroPos,Range(..))
import Dang.Syntax.Parser (lexWithLayout)

import           Control.Monad (replicateM_)
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L
import qualified System.Console.ANSI as Term


-- | Print out a formatted chunk of source code to the console.
formatChunk :: Source -> Position -> L.Text -> IO ()
formatChunk src start chunk =
  do spaces (posCol start - 1)
     go start toks

  where

  toks = lexWithLayout src (Just start) chunk

  spaces   n = putStr (replicate (fromIntegral n) ' ')
  newlines n = replicateM_ (fromIntegral n) (putStrLn "")

  go pos (Located { locRange = Range { .. }, .. } : toks) =
      do if posRow rangeStart == posRow pos
            then spaces (posCol rangeStart - posCol pos)
            else do newlines (posRow rangeStart - posRow pos)
                    spaces (posCol rangeStart - 1)
         formatToken locValue
         go rangeEnd toks

  go _ [] = putStrLn ""


test = formatChunk Interactive zeroPos
  "module Foo : sig\n\
  \               foo : Int\n\
  \           = struct\n\
  \               foo = 10"


formatToken :: Token -> IO ()
formatToken (TUnqualCon c)    =    L.putStr c
formatToken (TQualCon ns n)   = do L.putStr ns
                                   putStr "."
                                   L.putStr n
formatToken (TUnqualIdent n)  =    L.putStr n
formatToken (TQualIdent ns n) = do L.putStr ns
                                   putStr "."
                                   L.putStr n
formatToken (TKeyword kw)     = formatKeyword kw
-- XXX handle other bases
formatToken (TNum b i)        = putStr (show i)

formatToken TStart            = return ()
formatToken TSep              = return ()
formatToken TEnd              = return ()
formatToken TError            = return ()


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
