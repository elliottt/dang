{-# LANGUAGE Safe #-}

module Dang.Syntax.Lexeme where

import           Dang.Utils.Location
import qualified Dang.Utils.Panic as Panic
import           Dang.Utils.Pretty

panic :: PPDoc -> a
panic  = Panic.panic "Dang.Syntax.Lexeme"


-- Positions -------------------------------------------------------------------

type Lexeme = Located Token

data Token = TKeyword Keyword
           | TVirt Virt
           | TConIdent String
           | TIdent String
           | TOperIdent String
           | TInt Integer Int
           | TEof
           | TError String
             deriving (Eq,Show)

data Keyword = Klet
             | Kin
             | Kwhere
             | Kmodule
             | Kopen
             | Kas
             | Khiding
             | Kpublic
             | Kprivate
             | Kforall
             | Kprimitive
             | Ktype
             | Kdata
             | Kcase
             | Kof
             | Krec
             | Klocal

               -- symbols
             | Klambda
             | Kassign
             | Krarrow | KRarrow
             | Klarrow
             | Klparen
             | Krparen
             | Klbracket
             | Krbracket
             | Klbrace
             | Krbrace
             | Kcomma
             | Ksemi
             | Kcolon
             | Kdot
             | Kpipe
             | Kunderscore
               deriving (Show,Eq)

data Virt = Vopen | Vsep | Vclose
            deriving (Show,Eq)


isEof :: Token -> Bool
isEof TEof = True
isEof _    = False

fromTConIdent :: Token -> String
fromTConIdent tok = case tok of
  TConIdent s -> s
  _           -> panic (text "expected TConIdent")

fromTIdent :: Token -> String
fromTIdent tok = case tok of
  TIdent s -> s
  _        -> panic (text "expected TIdent")


instance Pretty Token where
  ppr tok = case tok of
    TVirt v      -> ppr v
    TKeyword k   -> ppr k
    TConIdent c  -> text c
    TIdent i     -> text i
    TOperIdent o -> text o
    TInt i _     -> integer i
    TEof         -> text "[EOF]"
    TError str   -> brackets (text "error:" <+> text str)

kw :: String -> PPDoc
kw n = withGraphics [fg green, bold, underscore] (text n)

kw2 :: String -> PPDoc
kw2 n = withGraphics [fg blue, bold, underscore] (text n)

sym :: String -> PPDoc
sym n = withGraphics [fg yellow] (text n)

instance Pretty Keyword where
  ppr x = case x of
    Klet        -> kw   "let"
    Kin         -> kw   "in"
    Kwhere      -> kw   "where"
    Kmodule     -> kw   "module"
    Kopen       -> kw2  "open"
    Kas         -> kw2  "as"
    Khiding     -> kw2  "hiding"
    Kpublic     -> kw2  "public"
    Kprivate    -> kw2  "private"
    Kforall     -> kw   "forall"
    Kprimitive  -> kw   "primitive"
    Ktype       -> kw   "type"
    Kdata       -> kw2  "data"
    Kcase       -> kw   "case"
    Kof         -> kw   "of"
    Krec        -> kw   "rec"
    Klocal      -> kw   "local"
    Klambda     -> sym  "\\"
    Kassign     -> sym  "="
    Klarrow     -> sym  "<-"
    Krarrow     -> sym  "->"
    KRarrow     -> sym  "=>"
    Klparen     -> char '('
    Krparen     -> char ')'
    Klbracket   -> char '['
    Krbracket   -> char ']'
    Klbrace     -> sym  "{"
    Krbrace     -> sym  "}"
    Kcomma      -> char ','
    Ksemi       -> char ';'
    Kcolon      -> sym  ":"
    Kdot        -> char '.'
    Kpipe       -> sym  "|"
    Kunderscore -> char '_'

instance Pretty Virt where
  ppr virt = case virt of
    Vopen  -> text "v{"
    Vsep   -> text "v;"
    Vclose -> text "v}"
