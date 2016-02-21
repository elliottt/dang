{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.Syntax.Format where

import Dang.Message (Message(..),MessageType(..),describeMessageType)
import Dang.Syntax.Lexer (lexer,Token(..),Keyword(..))
import Dang.Syntax.Location
           (Source(..),Located(..),Position(..),Range(..),zeroPos,rangeText
           ,rangeUnderline)
import Dang.Utils.PP

import           Data.Int (Int64)
import qualified Data.Text.Lazy as L


formatMessage :: Source -> L.Text -> Message -> Doc
formatMessage src txt (Message ty loc doc) = vcat
  [ ppHeading (show (tyDoc <+> pp src <+> pp loc))
  , text ""
  , describeMessageType ty
  , text ""
  , chunk
  , nest gutterLen (rangeUnderline loc)
  , text ""
  , doc ]
  where
  (chunk,gutterLen) = formatChunk src startPos (rangeText cxtLines loc txt)

  cxtLines = 3

  tyDoc = case ty of
    Error{}   -> text "[error]"
    Warning{} -> text "[warning]"

  startRow = posRow (rangeStart loc) - fromIntegral cxtLines
  startPos = zeroPos { posRow = max 1 startRow }

  ppHeading msg =
    text "--" <+> text msg <+> text (replicate (80 - length msg - 4) '-')


-- | Draw the space defined by two positions. When a new line is started, invoke
-- the function given to define the gutter.
spaceBetween :: Int64 -> (Int64 -> Doc)
             -> Position -> Position -> Doc
spaceBetween gutterLen mkGutter = \ start end ->
  let spansMultipleLines = posRow start < posRow end

      newlines
        | spansMultipleLines =
          text "" $+$
          nest (negate (fromIntegral (posCol start + gutterLen)))
            (vcat [ mkGutter i | i <- [ posRow start + 1 .. posRow end ] ])

        | otherwise =
          emptyDoc

      spaces
        | spansMultipleLines =
          text (replicate (fromIntegral (posCol end) - 1) ' ')

        | otherwise =
          text (replicate (fromIntegral (posCol end - posCol start)) ' ')

   in newlines <> spaces
{-# INLINE spaceBetween #-}


-- | Print out a formatted chunk of source code to the console. The returned
-- value is the size of the line number gutter.
formatChunk :: Source -> Position -> L.Text -> (Doc,Int)
formatChunk src start chunk = (prefix <> go start toks, pad + 1)
  where

  toks = lexer src (Just start) chunk

  pad = length (show (posRow (rangeEnd (locRange (last toks)))))

  gutter row =
    let str = show row
        num = text $ showString (replicate (pad - length str) ' ')
                   $ showString str ""

     in num <> annotate AnnPunc (char '|')

  moveTo = spaceBetween (fromIntegral pad) gutter

  prefix = moveTo start { posRow = posRow start - 1 } start

  go pos (Located { .. }:ts) =
    moveTo pos (rangeStart locRange)
    <> formatToken locValue <> go (rangeEnd locRange) ts

  go _ [] = emptyDoc


ppQual :: [L.Text] -> L.Text -> Doc
ppQual ns n = pp (L.intercalate "." ns) <> char '.' <> pp n

formatToken :: Token -> Doc
formatToken (TUnqualCon c)    = pp c
formatToken (TQualCon ns n)   = ppQual ns n
formatToken (TUnqualIdent n)  = pp n
formatToken (TQualIdent ns n) = ppQual ns n
formatToken (TKeyword kw)     = formatKeyword kw
formatToken (TLineComment l)  = annotate AnnComment (pp l)
-- XXX handle other bases
formatToken (TNum _ i)        = annotate AnnLiteral (pp i)

formatToken TStart            = emptyDoc
formatToken TSep              = emptyDoc
formatToken TEnd              = emptyDoc
formatToken (TError s)        = pp s

formatKeyword :: Keyword -> Doc
formatKeyword Kmodule  = annotate AnnKeyword   (text "module")
formatKeyword Kfunctor = annotate AnnKeyword   (text "functor")
formatKeyword Ksig     = annotate AnnKeyword   (text "sig")
formatKeyword Kstruct  = annotate AnnKeyword   (text "struct")
formatKeyword Kwhere   = annotate AnnKeyword   (text "where")
formatKeyword Kcolon   = annotate AnnPunc      (text ":")
formatKeyword Kimport  = annotate AnnKeyword   (text "import")
formatKeyword Kopen    = annotate AnnKeyword   (text "open")
formatKeyword Klparen  =                        text "("
formatKeyword Krparen  =                        text ")"
formatKeyword Krarrow  = annotate AnnPunc      (text "->")
formatKeyword Kassign  = annotate AnnPunc      (text "=")
formatKeyword Ktype    = annotate AnnKeyword   (text "type")
formatKeyword Kforall  = annotate AnnKeyword   (text "forall")
formatKeyword Kdot     = annotate AnnPunc      (text ".")
formatKeyword Kcomma   =                        text ","
formatKeyword Kwild    =                        text "_"
formatKeyword Kpipe    = annotate AnnPunc      (text "|")
formatKeyword Klet     = annotate AnnPunc      (text "let")
formatKeyword Kin      = annotate AnnPunc      (text "in")
