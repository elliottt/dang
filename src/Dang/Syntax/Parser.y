{
-- vim: ft=haskell

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

module Dang.Syntax.Parser where

import Dang.ModuleSystem.Export
import Dang.ModuleSystem.QualName
import Dang.ModuleSystem.Types
import Dang.Syntax.AST
import Dang.Syntax.Lexeme
import Dang.Syntax.ParserCore
import Dang.Utils.Location (Located(..),unLoc,getLoc,at,ppLoc,extendLoc)
import Dang.Utils.Pretty

import Data.Foldable ( foldMap )
import Data.Monoid (mempty,mappend,mconcat)
import MonadLib
}

%token

-- modules
  'module'    { Located $$ (TKeyword Kmodule)    }
  'open'      { Located $$ (TKeyword Kopen)      }
  'as'        { Located $$ (TKeyword Kas)        }
  'hiding'    { Located $$ (TKeyword Khiding)    }

-- declaration blocks
  '{'       { Located $$ (TKeyword Klbrace)    }
  ';'       { Located $$ (TKeyword Ksemi)      }
  '}'       { Located $$ (TKeyword Krbrace)    }
  'v{'      { Located $$ (TVirt Vopen)         }
  'v;'      { Located $$ (TVirt Vsep)          }
  'v}'      { Located $$ (TVirt Vclose)        }
  'rec'     { Located $$ (TKeyword Krec)       }
  'public'  { Located $$ (TKeyword Kpublic)    }
  'private' { Located $$ (TKeyword Kprivate)   }
  'local'   { Located $$ (TKeyword Klocal)     }

-- declarations
  'primitive' { Located $$ (TKeyword Kprimitive) }
  'type'      { Located $$ (TKeyword Ktype)      }
  'data'      { Located $$ (TKeyword Kdata)      }
  '='         { Located $$ (TKeyword Kassign)    }
  ':'         { Located $$ (TKeyword Kcolon)     }

-- type-related
  '->' { Located $$ (TOperIdent "->") }
  '=>' { Located $$ (TOperIdent "=>") }
  '*'  { Located $$ (TOperIdent "*")  }

-- keywords
  'where'     { Located $$ (TKeyword Kwhere)      }
  'case'      { Located $$ (TKeyword Kcase)       }
  'of'        { Located $$ (TKeyword Kof)         }
  'let'       { Located $$ (TKeyword Klet)        }
  'in'        { Located $$ (TKeyword Kin)         }
  '\\'        { Located $$ (TKeyword Klambda)     }
  '('         { Located $$ (TKeyword Klparen)     }
  ')'         { Located $$ (TKeyword Krparen)     }
  ','         { Located $$ (TKeyword Kcomma)      }
  '.'         { Located $$ (TKeyword Kdot)        }
  '|'         { Located $$ (TKeyword Kpipe)       }
  '_'         { Located $$ (TKeyword Kunderscore) }

-- identifiers
  CONIDENT { $$@Located { locValue = TConIdent _  }}
  IDENT    { $$@Located { locValue = TIdent _     }}
  OPER     { $$@Located { locValue = TOperIdent _ }}
  INT      { $$@Located { locValue = TInt _ _     }}


%monad { Parser } { (>>=) } { return }
%error { parseError }

%name parseModule top_module

%tokentype { Lexeme }

%lexer { lexer } { Located mempty TEof }

%%

-- Names -----------------------------------------------------------------------

cident :: { Located String }
  : CONIDENT { fmap fromTConIdent $1 }

ident :: { Located String }
  : IDENT { fmap fromTIdent $1 }
  | 'as'  { "as" `at` $1 }

qual_cident :: { Located [String] }
  : sep1('.', cident) { map unLoc $1 `at` foldMap getLoc $1 }

mod_name :: { Located ModName }
  : qual_cident { $1 }

qual_ident :: { Located ([String],String) }
  : sep1('.', cident) '.' ident
    { (map unLoc $1, unLoc $3) `at` (foldMap getLoc $1 `mappend` getLoc $3) }
  | ident
    { fmap ([],) $1 }


-- Modules ---------------------------------------------------------------------

top_module :: { Module }
  : 'module' mod_name 'where' block(top_decl)
    { Module { modName  = $2
             , modDecls = $4 } }


-- Declaration Blocks ----------------------------------------------------------

block(e)
  : layout(block_stmt(e))
    { BLoc (foldr BComb BEmpty $1 `at` getLoc $1) }

block_stmt(e)
  : e
    { BLoc (BSingle $1 `at` getLoc $1) }

  | 'rec' block(e)
    { BLoc (BRec $2 `at` mappend $1 (getLoc $2)) }

  | 'local' block(e) 'in' block(e)
    { BLoc (BLocal $2 $4 `at` mconcat [$1,$3,getLoc $4]) }


-- Declarations ----------------------------------------------------------------

top_decl :: { TopDecl }
  : decl { TDDecl $1 }

decl :: { Decl }
  : open      { DOpen $1 }
  | signature { DSig $1 }


-- Imports ---------------------------------------------------------------------

open :: { Located Open }
  : 'open' mod_name opt_as opt_hiding open_symbols
    { Open { openMod     = $2
           , openAs      = $3
           , openHiding  = $4
           , openSymbols = $5 } `at` mconcat [ $1
                                             , getLoc $2
                                             , getLoc $3
                                             , getLoc $5 ] }

opt_as :: { Maybe (Located ModName) }
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


-- Expressions -----------------------------------------------------------------

ename :: { Located Name }
  : ident { fmap (mkLocal Expr) $1 }



-- Types -----------------------------------------------------------------------

signature :: { Located Signature }
  : sep1(',', ename) ':' schema
    { Signature { sigNames  = $1
                , sigSchema = $3 } `at` mconcat [ getLoc $1
                                                , $2
                                                , getLoc $3 ] }

schema :: { Schema }
  : type '=>' type
    { mkForall $1 $3 }

  | type
    { Forall [] $1 }

type :: { Type }
  : sep1('->', app_type)
    { TLoc (foldr1 TFun $1 `at` getLoc $1) }

app_type :: { Type }
  : atype
    { $1 }

  | atype list1(atype)
    { TLoc (TApp $1 $2 `at` mappend (getLoc $1) (getLoc $2)) }

atype :: { Type }
  : tyvar
    { TLoc (TVar `fmap` $1) }

  | tycon
    { TLoc (TCon `fmap` $1) }

  | '(' sep(',', type) ')'
    { TLoc (mkTuple $2 `at` mappend (getLoc $1) (getLoc $3)) }

tyvar :: { Located Name }
  : ident { fmap (mkLocal (Type 0)) $1 }

tycon :: { Located Name }
  : cident { fmap (mkLocal (Type 0)) $1 }


-- Combinators -----------------------------------------------------------------

layout(e)
  : '{'  sep(';', e)  '}'  { $2 }
  | 'v{' sep('v;', e) 'v}' { $2 }

layout1(e)
  : '{'  sep1(';', e)  '}'  { $2 }
  | 'v{' sep1('v;', e) 'v}' { $2 }


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
