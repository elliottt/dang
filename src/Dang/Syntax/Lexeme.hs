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

               -- symbols
             | Klambda
             | Kassign
             | Klparen
             | Krparen
             | Klbracket
             | Krbracket
             | Klbrace
             | Krbrace
             | KfatArrow
             | Karrow
             | Kcomma
             | Ksemi
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

instance Pretty Keyword where
  ppr kw = case kw of
    Klet        -> text "let"
    Kin         -> text "in"
    Kwhere      -> text "where"
    Kmodule     -> text "module"
    Kopen       -> text "open"
    Kas         -> text "as"
    Khiding     -> text "hiding"
    Kpublic     -> text "public"
    Kprivate    -> text "private"
    Kforall     -> text "forall"
    Kprimitive  -> text "primitive"
    Ktype       -> text "type"
    Kdata       -> text "data"
    Kcase       -> text "case"
    Kof         -> text "of"
    Klambda     -> char '\\'
    Kassign     -> char '='
    Klparen     -> char '('
    Krparen     -> char ')'
    Klbracket   -> char '['
    Krbracket   -> char ']'
    Klbrace     -> char '{'
    Krbrace     -> char '}'
    KfatArrow   -> text "=>"
    Karrow      -> text "->"
    Kcomma      -> char ','
    Ksemi       -> char ';'
    Kdot        -> char '.'
    Kpipe       -> char '|'
    Kunderscore -> char '_'

instance Pretty Virt where
  ppr virt = case virt of
    Vopen  -> text "v{"
    Vsep   -> text "v;"
    Vclose -> text "v}"
