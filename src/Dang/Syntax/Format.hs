{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.Syntax.Format where

import Dang.Message (Message(..),MessageType(..),describeMessageType)
import Dang.Syntax.Lexer (lexer,Token(..),Keyword(..),Lexeme(..))
import Dang.Syntax.Location (Source,SourceRange(..),SourcePos(..),startPos)
import Dang.Utils.PP

import           Data.List (intersperse)
import qualified Data.Text as T

import Debug.Trace


formatMessage :: Source -> T.Text -> Message -> Doc
formatMessage src txt (Message ty loc doc) = vcat
  [ annotate msgAnn (ppHeading (show (tyDoc <+> pp src <+> pp loc)))
  , text ""
  , describeMessageType ty
  , text ""
  , source
  , doc
  , text "" ]
  where
  (chunk,gutterLen) = formatChunk src start (rangeText cxtLines loc txt)

  source | T.null txt = emptyDoc
         | otherwise  = chunk
                     $$ nest gutterLen (rangeUnderline msgAnn loc)
                     $$ text ""

  cxtLines = 3

  (tyDoc,msgAnn) = case ty of
    Error{}   -> (text "[error]",   AnnError)
    Warning{} -> (text "[warning]", AnnWarning)

  startRow = sourceLine (sourceFrom loc) - fromIntegral cxtLines
  start = (startPos src) { sourceColumn = max 1 startRow }

  ppHeading msg =
    text "--" <+> text msg <+> text (replicate (80 - length msg - 4) '-')


-- | Extract the range of text with n context lines, centered around the range
-- provided, from the program text.
rangeText ::
  Int         {-^ Context lines -} ->
  SourceRange {-^ Start region-} ->
  T.Text      {-^ Source text -} ->
  T.Text

rangeText cxt SourceRange { .. } txt
  = T.unlines
  $ take keep
  $ drop skip
  $ T.lines txt

  where
  skip = max 0 (sourceLine sourceFrom - cxt)

  keep = sourceLine sourceTo - sourceLine sourceFrom

-- | Generate a single underline for the range specified.
rangeUnderline :: Ann -> SourceRange -> Doc
rangeUnderline ann SourceRange { .. } =
  text (replicate (start - 1) ' ') <> annotate ann (text line)
  where
  start = fromIntegral (sourceColumn sourceFrom)
  end   = fromIntegral (sourceColumn sourceTo)

  len   = end - start

  line | len > 1   = replicate len '~'
       | otherwise = "^"


-- | Draw the space defined by two positions. When a new line is started, invoke
-- the function given to define the gutter.
spaceBetween :: Int -> (Int -> Doc)
             -> SourcePos -> SourcePos -> Doc
spaceBetween gutterLen mkGutter = \ start end ->
  let spansMultipleLines = sourceLine start < sourceLine end

      newlines
        | spansMultipleLines =
          text "" $+$
          nest (negate (fromIntegral (sourceColumn start + gutterLen)))
            (vcat [ mkGutter i | i <- [ sourceLine start + 1 .. sourceLine end ] ])

        | otherwise =
          emptyDoc

      spaces
        | spansMultipleLines =
          text (replicate (fromIntegral (sourceColumn end) - 1) ' ')

        | otherwise =
          text (replicate (fromIntegral (sourceColumn end - sourceColumn start)) ' ')

   in newlines <> spaces
{-# INLINE spaceBetween #-}


-- | Print out a formatted chunk of source code to the console. The returned
-- value is the size of the line number gutter.
formatChunk :: Source -> SourcePos -> T.Text -> (Doc,Int)
formatChunk src start chunk
  | T.null chunk = (emptyDoc,0)
  | otherwise    = (prefix <> go start toks, pad + 1)
  where

  toks = lexer src (Just start) chunk

  pad = length (show (sourceLine (sourceTo (lexemeRange (last toks)))))

  gutter row =
    let str = show row
        num = text $ showString (replicate (pad - length str) ' ')
                   $ showString str ""

     in annotate AnnGutter (num <> char '|')

  moveTo = spaceBetween (fromIntegral pad) gutter

  prefix = gutter (sourceLine start) <> moveTo start { sourceLine = 1 } start

  go pos (Lexeme { .. }:ts) =
    moveTo pos (sourceFrom lexemeRange)
    <> formatToken lexemeToken
    <> go (sourceTo lexemeRange) ts

  go _ [] = emptyDoc


ppQual :: [T.Text] -> T.Text -> Doc
ppQual ns n = hcat (map pp (intersperse (T.pack ".") ns)) <> char '.' <> pp n

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
formatKeyword Krequire = annotate AnnKeyword   (text "require")
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
