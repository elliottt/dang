{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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


instance Pretty Keyword where
  ppr x = case x of
    Klet        -> exprKw "let"
    Kin         -> exprKw "in"
    Kforall     -> exprKw "forall"
    Kcase       -> exprKw "case"
    Kof         -> exprKw "of"

    Kwhere      -> declKw "where"
    Kmodule     -> declKw "module"
    Kopen       -> declKw "open"
    Kas         -> declKw "as"
    Khiding     -> declKw "hiding"
    Kpublic     -> declKw "public"
    Kprivate    -> declKw "private"
    Kprimitive  -> declKw "primitive"
    Ktype       -> declKw "type"
    Kdata       -> declKw "data"
    Krec        -> declKw "rec"
    Klocal      -> declKw "local"

    Klambda     -> sym  "\\"
    Kassign     -> sym  "="
    Klarrow     -> sym  "<-"
    Krarrow     -> sym  "->"
    KRarrow     -> sym  "=>"
    Klbrace     -> sym  "{"
    Krbrace     -> sym  "}"
    Kcolon      -> sym  ":"
    Kpipe       -> sym  "|"

    Klparen     -> char '('
    Krparen     -> char ')'
    Klbracket   -> char '['
    Krbracket   -> char ']'
    Kcomma      -> char ','
    Ksemi       -> char ';'
    Kdot        -> char '.'
    Kunderscore -> char '_'

    where
    exprKw n = annotate (Keyword ExprKw) (text n)
    declKw n = annotate (Keyword DeclKw) (text n)
    sym    n = annotate (Keyword SymKw)  (text n)

instance Pretty Virt where
  ppr virt = case virt of
    Vopen  -> text "v{"
    Vsep   -> text "v;"
    Vclose -> text "v}"
