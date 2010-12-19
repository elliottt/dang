{
{-# OPTIONS_GHC -w #-}

-- vim: filetype=haskell

module Syntax.Parser where

import QualName
import Syntax.AST
import Syntax.Lexer
import Syntax.ParserCore

import MonadLib
import qualified Codec.Binary.UTF8.Generic as UTF8
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
  '.'  { Lexeme $$ (TReserved ".")  }

-- reserved names
  'module' { Lexeme $$ (TReserved "module") }
  'where'  { Lexeme $$ (TReserved "where")  }

-- identifiers
  CONIDENT { Lexeme _ (TConIdent $$) }
  IDENT    { Lexeme _ (TSymIdent $$) }
  INT      { Lexeme _ (TInt $$)      }


%monad { Parser } { (>>=) } { return }
%name parseModule top_module
%name parseFunBind fun_bind
%name parseFunBinds fun_binds
%tokentype { Lexeme }

%lexer { lexer } { Lexeme initPosition TEof }

%%

top_module :: { Module }
  : 'module' mod_name 'where' '{' fun_binds '}' { Module $2 $5 }

mod_name :: { QualName }
  : qual_name_prefix '.' CONIDENT { QualName (reverse $1) $3 }
  | CONIDENT                      { QualName [] $1 }

qual_name :: { QualName }
  : qual_name_prefix '.' IDENT { QualName (reverse $1) $3 }
  | IDENT                      { QualName [] $1 }

qual_name_prefix :: { [Name] }
  : qual_name_prefix '.' CONIDENT { $3:$1 }
  | CONIDENT                      { [$1] }

fun_bind :: { Decl }
  : IDENT arg_list '=' exp { Decl $1 (reverse $2) True $4 }

arg_list :: { [String] }
  : arg_list IDENT { $2 : $1 }
  | {- empty -}    { [] }

fun_binds :: { [Decl] }
  : fun_binds ';' fun_bind { $3 : $1 }
  | fun_bind               { [$1]    }

exp :: { Term }
  : '\\' abs_args '->' lexp { Abs (reverse $2) $4 }
  | lexp                    { $1 }

abs_args :: { [String] }
  : abs_args IDENT { $2 : $1 }
  | IDENT          { [$1] }

lexp :: { Term }
  : 'let' '{' fun_binds '}' 'in' fexp { Let (reverse $3) $6 }
  | fexp                              { $1 }

fexp :: { Term }
  : aexp aexp_list { apply $1 (reverse $2) }

aexp_list :: { [Term] }
  : aexp_list aexp { $2 : $1 }
  | {- empty -}    { [] }

aexp :: { Term }
  : '(' exp ')' { $2 }
  | IDENT       { Var $1 }
  | INT         { Lit (LInt $1) }

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = scan >>= k

happyError :: Parser a
happyError  = raiseP "Parse error"
}
