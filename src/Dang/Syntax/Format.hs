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
import           Data.Function (on)
import           Data.List (groupBy)
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L


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
  (chunk,gutterLen) = formatChunk src startPos Nothing (rangeText cxtLines loc txt)

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
formatChunk :: Source -> Position -> Maybe Range -> L.Text -> (Doc,Int)
formatChunk src start mbErr chunk = (go beginRow ls, pad + 1)
  where

  beginRow = start { posCol = 1
                   , posOff = posOff start - posCol start + 1 }

  startRow Located { locRange = Range { .. } } = posRow rangeStart

  toks = lexer src (Just start) chunk
  ls   = groupBy ((==) `on` startRow) toks

  pad = length (show (posRow (rangeEnd (locRange (last toks)))))

  gutter row = text $
    let str = show row
     in showString (replicate (pad - length str) ' ')
      $ showString str
      $ showChar '|' ""

  firstPos (Located { locRange = Range { .. } } :_) = rangeStart

  go pos (g:gs) = vcat (blanks ++ [gutter (posRow s) <> line, go end' gs ])
    where
    s = firstPos g

    blanks = map gutter [posCol pos .. posCol s - posCol pos - 1]

    (line,end) = foldl group (emptyDoc,pos) g

    end' = end { posRow = posRow end + 1, posCol = 1 }


  go _ [] = emptyDoc

  spaces a b = text (replicate (fromIntegral (posCol b - posCol a)) ' ')

  group (acc,pos) Located { locRange = Range { .. }, .. } =
    (acc <> spaces pos rangeStart <> formatToken locValue, rangeEnd)


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
formatToken (TNum b i)        = annotate AnnLiteral (pp i)

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
