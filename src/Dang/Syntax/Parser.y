{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE Trustworthy #-}

-- vim: ft=haskell

module Dang.Syntax.Parser where

import Dang.ModuleSystem.Export
import Dang.ModuleSystem.QualName
import Dang.ModuleSystem.Types
import Dang.Syntax.AST
import Dang.Syntax.Lexeme
import Dang.Syntax.ParserCore
import Dang.Utils.Location (Located(..),unLoc,getLoc,at,ppLoc,extendLoc)
import Dang.Utils.Pretty

import Data.Foldable (foldMap)
import Data.Monoid (mempty,mappend,mconcat)
import MonadLib
}

%token

-- reserved names
  'let' { Located $$ (TReserved "let") }
  'in'  { Located $$ (TReserved "in")  }

-- symbols
  '\\' { Located $$ (TReserved "\\") }
  '='  { Located $$ (TReserved "=")  }
  '('  { Located $$ (TReserved "(")  }
  ')'  { Located $$ (TReserved ")")  }
  '{'  { Located $$ (TReserved "{")  }
  '}'  { Located $$ (TReserved "}")  }
  ';'  { Located $$ (TReserved ";")  }
  ','  { Located $$ (TReserved ",")  }
  '.'  { Located $$ (TReserved ".")  }
  '|'  { Located $$ (TReserved "|")  }
  '_'  { Located $$ (TReserved "_")  }

-- special operators
  '->' { Located $$ (TOperIdent "->") }
  '*'  { Located $$ (TOperIdent "*")  }
  '::' { Located $$ (TOperIdent "::") }

-- reserved names
  'module'    { Located $$ (TReserved "module")    }
  'where'     { Located $$ (TReserved "where")     }
  'open'      { Located $$ (TReserved "open")      }
  'as'        { Located $$ (TReserved "as")        }
  'hiding'    { Located $$ (TReserved "hiding")    }
  'public'    { Located $$ (TReserved "public")    }
  'private'   { Located $$ (TReserved "private")   }
  'primitive' { Located $$ (TReserved "primitive") }
  'type'      { Located $$ (TReserved "type")      }
  'data'      { Located $$ (TReserved "data")      }
  'case'      { Located $$ (TReserved "case")      }
  'of'        { Located $$ (TReserved "of")        }

-- identifiers
  CONIDENT { Located _ (TConIdent _)  }
  IDENT    { Located _ (TSymIdent _)  }
  OPER     { Located _ (TOperIdent _) }
  INT      { Located _ (TInt _)       }


%monad { Parser } { (>>=) } { return }
%error { parseError }

%name parseModule top_module

%tokentype { Lexeme }

%lexer { lexer } { Located mempty TEof }

%%

-- Names -----------------------------------------------------------------------

cident :: { Located Name }
  : CONIDENT { let TConIdent n = locValue $1 in fmap (const n) $1 }

ident :: { Located Name }
  : IDENT { let TSymIdent n = locValue $1 in fmap (const n) $1 }

mod_name :: { Located Name }
  : qual_cident { $1 }

qual_cident :: { Located Name }
  : sep1('.', cident) { $1 }

qual_ident :: { Located Name }
  : sep1('.', cident) '.' ident
    { mkQual (map locValue $1) (locValue $3)
          `at` mappend (mconcat (map getLoc $1)) (getLoc $3) }
  | ident
    { fmap mkLocal $1 }


-- Modules ---------------------------------------------------------------------

top_module :: { Module }
  : 'module' mod_name '{' top_decls '}'
    { Module { modName  = $2
             , modOpens = fst $4
             , modDecls = snd $4 } }

top_decls :: { ([Open],Block TopDecl) }
  : sep1(';', open) { ($1, []) }

open :: { Open }
  : 'open' mod_name opt_as opt_hiding open_symbols
    { Open { openMod     = $2
           , openAs      = $3
           , openHiding  = $4
           , openSymbols = map unLoc $5 } `at` mconcat [ $1
                                                       , getLoc $2
                                                       , getLoc $3
                                                       , getLoc $5 ] }

opt_as :: { Maybe Name }
  : 'as' mod_name { Just $2 }
  | {- empty -}   { Nothing }

opt_hiding :: { Bool }
  : 'hiding'    { True  }
  | {- empty -} { False }

open_symbols :: { [Located OpenSymbol] }
  : '(' sep1(',', open_symbol) ')' { $2 }
  | {- empty -}                    { [] }

open_symbol :: { Located OpenSymbol }
  : ident
    { fmap OpenTerm $1 }
  | cident '(' sep1(',',cident) ')'
    { OpenType (unLoc $1) (map unLoc $3)
          `at` mconcat (getLoc $1 : map getLoc $3) }


-- Combinators -----------------------------------------------------------------

sep(p,q)
  : sep_body(p,q) { reverse $1 }
  | {- empty -}   { []         }

sep1(p,q)
  : sep_body(p,q) { reverse $1 }

sep_body(p,q)
  : sep_body(p,q) p q { $3 : $1 }
  | q                 { [$1]    }


list(p)
  : list_body(p) { reverse $1 }
  | {- empty -}  { []         }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
  : list_body(p) p { $2 : $1 }
  | p              { [$1]    }
