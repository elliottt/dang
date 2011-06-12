{
{-# OPTIONS_GHC -w #-}

-- vim: filetype=haskell

module Syntax.Parser where

import QualName
import Syntax.AST
import Syntax.Lexer
import Syntax.ParserCore
import TypeChecker.Types

import MonadLib
import qualified Codec.Binary.UTF8.Generic as UTF8

import Debug.Trace
}

%token

-- reserved names
  'let' { Lexeme $$ (TReserved "let") }
  'in'  { Lexeme $$ (TReserved "in")  }

-- symbols
  '\\' { Lexeme $$ (TReserved "\\") }
  '->' { Lexeme $$ (TReserved "->") }
  '='  { Lexeme $$ (TReserved "=")  }
  '('  { Lexeme $$ (TReserved "(")  }
  ')'  { Lexeme $$ (TReserved ")")  }
  '{'  { Lexeme $$ (TReserved "{")  }
  '}'  { Lexeme $$ (TReserved "}")  }
  ';'  { Lexeme $$ (TReserved ";")  }
  ','  { Lexeme $$ (TReserved ",")  }
  '.'  { Lexeme $$ (TReserved ".")  }
  '::' { Lexeme $$ (TReserved "::") }
  '=>' { Lexeme $$ (TReserved "=>") }
  '*'  { Lexeme $$ (TReserved "*")  }

-- reserved names
  'module'    { Lexeme $$ (TReserved "module")    }
  'where'     { Lexeme $$ (TReserved "where")     }
  'open'      { Lexeme $$ (TReserved "open")      }
  'as'        { Lexeme $$ (TReserved "as")        }
  'hiding'    { Lexeme $$ (TReserved "hiding")    }
  'public'    { Lexeme $$ (TReserved "public")    }
  'private'   { Lexeme $$ (TReserved "private")   }
  'forall'    { Lexeme $$ (TReserved "forall")    }
  'primitive' { Lexeme $$ (TReserved "primitive") }
  'type'      { Lexeme $$ (TReserved "type")      }

-- identifiers
  CONIDENT { Lexeme _ (TConIdent $$) }
  IDENT    { Lexeme _ (TSymIdent $$) }
  INT      { Lexeme _ (TInt $$)      }


%monad { Parser } { (>>=) } { return }
%name parseModule top_module
%name parseForall qual_type
%tokentype { Lexeme }

%lexer { lexer } { Lexeme initPosition TEof }

%%

-- Names -----------------------------------------------------------------------

qual_name :: { QualName }
  : qual_name_prefix '.' IDENT { QualName (reverse $1) $3 }
  | IDENT                      { QualName [] $1 }

mod_name :: { QualName }
  : qual_name_prefix '.' CONIDENT { QualName (reverse $1) $3 }
  | CONIDENT                      { QualName [] $1 }


-- Modules ---------------------------------------------------------------------

top_module :: { Module }
  : 'module' mod_name 'where' '{' top_decls '}' {% mkModule $2 $5 }


-- Declarations ----------------------------------------------------------------

top_decls :: { PDecls }
  : top_decls ';' top_decl      { $1 `combinePDecls` $3 }
  | top_decls ';' public_decls  { $1 `combinePDecls` $3 }
  | top_decls ';' private_decls { $1 `combinePDecls` $3 }
  | top_decl                    { $1 }
  | public_decls                { $1 }
  | private_decls               { $1 }

top_decl :: { PDecls }
  : top_fun_bind { mkDecl $1 }
  | open         { mkOpen $1 }
  | primitive    { $1 }

primitive :: { PDecls }
  : 'primitive' primitive_body { $2 }

primitive_body :: { PDecls }
  : IDENT           '::' qual_type { mkPrimTerm (PrimTerm $1 $3) }
  | 'type' CONIDENT '::' kind      { mkPrimType (PrimType $2 $4) }

public_decls :: { PDecls }
  : 'public' '{' fun_binds '}' { publicExport (mkDecls $3) }

private_decls :: { PDecls }
  : 'private' '{' fun_binds '}' { privateExport (mkDecls $3) }

qual_name_prefix :: { [Name] }
  : qual_name_prefix '.' CONIDENT { $3:$1 }
  | CONIDENT                      { [$1] }

top_fun_bind :: { Decl }
  : export_type fun_bind { $2 { declExport = $1 } }

fun_binds :: { [Decl] }
  : fun_binds ';' fun_bind { $3:$1 }
  | fun_bind               { [$1] }

fun_bind :: { Decl }
  : IDENT arg_list '=' exp { Decl Private Nothing $1 (reverse $2) $4 }

export_type :: { Export }
  : {- empty -} { Public }
  | 'public'    { Public }
  | 'private'   { Private }

open :: { Open }
  : 'open' mod_name open_body { $3 $2 }

open_body :: { QualName -> Open }
  : 'as' mod_name open_tail { \qn -> uncurry (Open qn (Just $2)) $3 }
  | open_tail               { \qn -> uncurry (Open qn Nothing)   $1 }

open_tail :: { (Bool,[Name]) }
  : {- empty -}                { (True, []) }
  | '(' mod_names ')'          { (False, $2) }
  | 'hiding' '(' mod_names ')' { (True, $3) }

mod_names :: { [Name] }
  : IDENT               { [$1] }
  | mod_names ',' IDENT { $3:$1 }

arg_list :: { [String] }
  : arg_list IDENT { $2 : $1 }
  | {- empty -}    { [] }


-- Terms -----------------------------------------------------------------------

exp :: { Term }
  : '\\' abs_args '->' lexp { Abs (reverse $2) $4 }
  | lexp                    { $1 }

abs_args :: { [String] }
  : abs_args IDENT { $2 : $1 }
  | IDENT          { [$1] }

lexp :: { Term }
  : 'let' '{' let_binds '}' 'in' fexp { Let (reverse $3) $6 }
  | fexp                              { $1 }

let_binds :: { [Decl] }
  : let_binds ';' fun_bind { $3:$1 }
  | fun_bind               { [$1]  }

fexp :: { Term }
  : aexp aexp_list { apply $1 (reverse $2) }

aexp_list :: { [Term] }
  : aexp_list aexp { $2 : $1 }
  | {- empty -}    { [] }

aexp :: { Term }
  : '(' exp ')' { $2 }
  | qual_name   { Global $1 }
  | INT         { Lit (LInt $1) }


-- Types -----------------------------------------------------------------------

type :: { Type }
  : atype type_tail { $2 $1 }

type_tail :: { Type -> Type }
  : {- empty -} { id }
  | '->' type   { \a -> tarrow a $2 }
  | type        { \a -> tapp a $1 }

atype :: { Type }
  : IDENT        { TVar 0 (TParam $1 setSort) }
  | CONIDENT     { TCon $1 }
  | INT          { TNat $1 }
  | '(' type ')' { $2 }

qual_type :: { Forall Type }
  : 'forall' tparams '.' type { Forall (reverse $2) $4 }

tparams :: { [TParam] }
  : tparams tparam { $2 : $1 }
  | tparam         { [$1] }

tparam :: { TParam }
  : IDENT { TParam $1 setSort }


-- Kinds -----------------------------------------------------------------------

kind :: { Kind }
  : akind kind_body { $2 $1 }

kind_body :: { Kind -> Kind }
  : {- empty -} { id }
  | '->' kind   { \a -> karrow a $2 }

akind :: { Kind }
  : '*'          { kstar }
  | '(' kind ')' { $2 }

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = scan >>= k

happyError :: Parser a
happyError  = raiseP "Parse error"

parseError tokens = raiseP "Parse thinger"
}
