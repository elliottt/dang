-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
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
  MOD_NAME { $$ @ Located { locValue = TModName _ } }
  UNQUAL   { $$ @ Located { locValue = TUnqual _  } }
  QUAL     { $$ @ Located { locValue = TQual _ _  } }

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
  : 'module' MOD_NAME 'where' 'v{' top_decls 'v}'
    { Module { modName  = mkModName $2
             , modDecls = $5 } }

top_decls :: { [Decl PName] } -- { ([Import],[Decl PName]) }
  : {- empty -} { []         }
  | decls       { reverse $1 }

-- Declarations ----------------------------------------------------------------

decls :: { [Decl PName] }
  : decl            { [$1]    }
  | decls 'v;' decl { $3 : $1 }

decl :: { Decl PName }
  : ident_commas ':' schema { DLoc (DSig (Sig (reverse $1) $3) `at` ($1,$3)) }


-- Types -----------------------------------------------------------------------

schema :: { Located (Schema PName) }
  : 'forall' idents '.' type { Schema (reverse $2) $4 `at` ($1,$3) }
  |                     type { Schema [] $1           `at`  $1     }

type :: { Type PName }
  : arr_type { mkTFun (reverse $1) }

arr_type :: { [Type PName] }
  : app_type               { [mkTApp (reverse $1)]    }
  | arr_type '->' app_type { mkTApp (reverse $3) : $1 }

app_type :: { [Type PName] }
  : atype          { [$1]    }
  | app_type atype { $2 : $1 }

atype :: { Type PName }
  : ident        { TLoc (TVar `fmap` $1) }
  | '(' type ')' { $2                    }


-- Names -----------------------------------------------------------------------

ident_commas :: { [Located PName] }
  : ident            { [$1]    }
  | idents ',' ident { $3 : $1 }

idents :: { [Located PName] }
  : ident        { [$1]    }
  | idents ident { $2 : $1 }

-- identifiers are unqualified parsed-names
ident :: { Located PName }
  : UNQUAL { case $1 of
               Located { locValue = TUnqual n, .. } -> PUnqual n `at` locRange }


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

mkModName :: Located Token -> Located Namespace
mkModName Located { .. } =
  case locValue of
    TModName ns -> Located { locValue = ns, .. }
    _           -> panic "parser" (text "mkModName: expected a TModName")


mkTApp :: [Type PName] -> Type PName
mkTApp [t]    = t
mkTApp (t:ts) = TLoc (TApp t ts `at` (t,ts))
mkTApp _      = panic "parser" (text "mkTApp: empty list")

mkTFun :: [Type PName] -> Type PName
mkTFun ts = TLoc (foldr1 TFun ts `at` ts)
}
