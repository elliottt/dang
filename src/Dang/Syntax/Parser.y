-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Dang.Syntax.Parser (
    parseModule
  ) where

import Dang.Monad
import Dang.Syntax.AST
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

  ':'      { Located $$ (TKeyword Kcolon)  }


%monad { Dang }
%error { parseError }

%name top_module

%%

top_module :: { PModule }
  : 'module' MOD_NAME 'where'
    { Module { modName  = mkModName $2
             , modDecls = [] } }

{

-- External Interface ----------------------------------------------------------

parseModule :: Source -> L.Text -> Dang PModule
parseModule src txt = top_module (lexer src txt)


-- Parser Monad ----------------------------------------------------------------

parseError :: [Located Token] -> Dang a
parseError  = error "parse error"


-- Utilities -------------------------------------------------------------------

mkModName :: Located Token -> Located Namespace
mkModName Located { .. } =
  case locValue of
    TModName ns -> Located { locValue = ns, .. }
    _           -> panic "parser" (text "mkModName: expected a TModName")

}
