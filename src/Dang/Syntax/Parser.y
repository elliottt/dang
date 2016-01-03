-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Dang.Syntax.Parser (
    parseModule,
    lexWithLayout
  ) where

import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Layout
import Dang.Syntax.Lexer
import Dang.Syntax.Location
import Dang.Utils.Ident
import Dang.Utils.PP (text)
import Dang.Utils.Panic

import qualified Data.Text.Lazy as L

}


%tokentype { Located Token }

%token
  QUAL_CON { $$ @ Located { locValue = TQualCon _ _    } }
  CON      { $$ @ Located { locValue = TUnqualCon _    } }
  UNQUAL   { $$ @ Located { locValue = TUnqualIdent _  } }
  QUAL     { $$ @ Located { locValue = TQualIdent _ _  } }

  'module' { Located $$ (TKeyword Kmodule) }
  'where'  { Located $$ (TKeyword Kwhere)  }

  'import' { Located $$ (TKeyword Kimport) }
  'open'   { Located $$ (TKeyword Kopen)   }
  'forall' { Located $$ (TKeyword Kforall) }

  '.'      { Located $$ (TKeyword Kdot)    }
  ','      { Located $$ (TKeyword Kcomma)  }

  ':'      { Located $$ (TKeyword Kcolon)  }
  '='      { Located $$ (TKeyword Kassign) }

  '->'     { Located $$ (TKeyword Krarrow) }

  '_'      { Located $$ (TKeyword Kwild)   }

  '('      { Located $$ (TKeyword Klparen) }
  ')'      { Located $$ (TKeyword Krparen) }

  'v{'     { Located $$ TStart }
  'v;'     { Located $$ TSep   }
  'v}'     { Located $$ TEnd   }


%monad { Dang }
%error { parseError }

%name top_module

%%


-- Top-level Module ------------------------------------------------------------

top_module :: { PModule }
  : 'module' mod_name 'where' 'v{' top_decls 'v}'
    { Module { modName  = $2
             , modDecls = $5 } }

top_decls :: { [Decl PName] } -- { ([Import],[Decl PName]) }
  : {- empty -}      { [] }
  | sep1('v;', decl) { $1 }

-- Declarations ----------------------------------------------------------------

decls :: { [Decl PName] }
  : decl            { [$1]    }
  | decls 'v;' decl { $3 : $1 }

decl :: { Decl PName }
  : signature { DLoc (DSig  `fmap` $1) }
  | bind      { DLoc (DBind `fmap` $1) }


-- Types -----------------------------------------------------------------------

signature :: { Located (Sig PName) }
  : sep1(',', ident) ':' schema { Sig $1 $3 `at` ($1,$3) }

schema :: { Located (Schema PName) }
  : 'forall' list1(ident) '.' type { Schema $2 $4 `at` ($1,$3) }
  |                           type { Schema [] $1 `at`  $1     }

type :: { Type PName }
  : sep1('->', app_type) { mkTFun $1 }

app_type :: { Type PName }
  : list1(atype) { mkTApp $1 }

atype :: { Type PName }
  : ident        { TLoc (TVar `fmap` $1) }
  | con_name     { TLoc (TCon `fmap` $1) }
  | '(' type ')' { $2                    }


-- Expressions -----------------------------------------------------------------

bind :: { Located (Bind PName) }
  : ident list(pat) '=' expr
    { Bind { bName   = $1
           , bSchema = Nothing
           , bBody   = addParams $2 $4 } `at` ($1,$4) }

pat :: { Pat PName }
  : '_'   { PLoc (PWild `at` $1) }
  | ident { PLoc (PVar  `fmap` $1) }

expr :: { Expr PName }
  : list1(aexpr) { mkEApp $1 }

aexpr :: { Expr PName }
  : ident        { ELoc (EVar `fmap` $1) }
  | qual_ident   { ELoc (EVar `fmap` $1) }
  | '(' expr ')' { $2                    }


-- Names -----------------------------------------------------------------------

mod_name :: { Located Namespace }
  : QUAL_CON { case thing $1 of
                 TQualCon ns n -> L.toStrict (L.concat [ns,".",n]) <$ $1 }
  | CON      { case thing $1 of TUnqualCon n -> L.toStrict n <$ $1 }

con_name :: { Located PName }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }
  | CON      { case thing $1 of TUnqualCon n  -> PUnqual n  <$ $1 }


qual_ident :: { Located PName }
  : QUAL { case thing $1 of TQualIdent ns n -> PQual ns n <$ $1 }

-- identifiers are unqualified parsed-names
ident :: { Located PName }
  : UNQUAL { case thing $1 of TUnqualIdent n -> PUnqual n <$ $1 }


-- Utilities -------------------------------------------------------------------

sep(p,q)
  : {- empty -}   { []         }
  | sep_body(p,q) { reverse $1 }

sep1(p,q)
  : sep_body(p,q) { reverse $1 }

sep_body(p,q)
  : q                 { [$1]    }
  | sep_body(p,q) p q { $3 : $1 }

list(p)
  : {- empty -}  { []         }
  | list_body(p) { reverse $1 }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
  : p              { [$1]    }
  | list_body(p) p { $2 : $1 }


-- External Interface ----------------------------------------------------------

{

lexWithLayout :: Source -> L.Text -> [Located Token]
lexWithLayout src txt = layout dangLayout (lexer src txt)
  where
  dangLayout =
    Layout { beginsLayout = (`elem` [TKeyword Kwhere])
           , endsLayout   = const False
           , start        = TStart
           , sep          = TSep
           , end          = TEnd
           }

parseModule :: Source -> L.Text -> Dang PModule
parseModule src txt = failErrors (top_module (lexWithLayout src txt))


-- Parser Monad ----------------------------------------------------------------

parseError :: [Located Token] -> Dang a
parseError toks =
  do case toks of
       loc : _ -> addLoc loc $ \case
         TError -> addError (text "Lexical error")
         _      -> addError (text "Parse error")

       [] -> addError (text "Unexpected end-of-file")

     mzero


-- Utilities -------------------------------------------------------------------

mkTApp :: [Type PName] -> Type PName
mkTApp [t]    = t
mkTApp (t:ts) = TLoc (TApp t ts `at` (t,ts))
mkTApp _      = panic "parser" (text "mkTApp: empty list")

mkTFun :: [Type PName] -> Type PName
mkTFun ts = TLoc (foldr1 TFun ts `at` ts)

mkEApp :: [Expr PName] -> Expr PName
mkEApp [e]    = e
mkEApp (e:es) = ELoc (EApp e es `at` (e,es))
mkEApp _      = panic "parser" (text "mkEApp: empty list")

addParams :: [Pat PName] -> Expr PName -> Match PName
addParams ps e = foldr MPat (MExpr e) ps
}
