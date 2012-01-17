{
{-# OPTIONS_GHC -w #-}

-- vim: filetype=haskell

module Syntax.Parser where

import ModuleSystem.Export
import ModuleSystem.Types
import QualName
import Syntax.AST
import Syntax.Lexeme
import Syntax.Lexer
import Syntax.ParserCore
import TypeChecker.Types

import Data.Monoid (mappend)
import MonadLib
}

%token

-- reserved names
  'let' { Lexeme $$ (TReserved "let") }
  'in'  { Lexeme $$ (TReserved "in")  }

-- symbols
  '\\' { Lexeme $$ (TReserved "\\") }
  '='  { Lexeme $$ (TReserved "=")  }
  '('  { Lexeme $$ (TReserved "(")  }
  ')'  { Lexeme $$ (TReserved ")")  }
  '{'  { Lexeme $$ (TReserved "{")  }
  '}'  { Lexeme $$ (TReserved "}")  }
  ';'  { Lexeme $$ (TReserved ";")  }
  ','  { Lexeme $$ (TReserved ",")  }
  '.'  { Lexeme $$ (TReserved ".")  }
  '|'  { Lexeme $$ (TReserved "|")  }

-- special operators
  '->' { Lexeme $$ (TOperIdent "->") }
  '*'  { Lexeme $$ (TOperIdent "*")  }
  '::' { Lexeme $$ (TOperIdent "::") }

-- reserved names
  'module'    { Lexeme $$ (TReserved "module")    }
  'where'     { Lexeme $$ (TReserved "where")     }
  'open'      { Lexeme $$ (TReserved "open")      }
  'as'        { Lexeme $$ (TReserved "as")        }
  'hiding'    { Lexeme $$ (TReserved "hiding")    }
  'public'    { Lexeme $$ (TReserved "public")    }
  'private'   { Lexeme $$ (TReserved "private")   }
  'primitive' { Lexeme $$ (TReserved "primitive") }
  'type'      { Lexeme $$ (TReserved "type")      }
  'data'      { Lexeme $$ (TReserved "data")      }
  'case'      { Lexeme $$ (TReserved "case")      }
  'of'        { Lexeme $$ (TReserved "of")        }

-- identifiers
  CONIDENT { Lexeme _ (TConIdent $$)  }
  IDENT    { Lexeme _ (TSymIdent $$)  }
  OPER     { Lexeme _ (TOperIdent $$) }
  INT      { Lexeme _ (TInt $$)       }


%monad { Parser } { (>>=) } { return }
%error { parseError }

%name parseModule top_module
%name parseTerm   exp
%name parseType   type
%name parseScheme qual_type

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
  : top_decls ';' top_decl      { $1 `mappend` $3 }
  | top_decls ';' public_decls  { $1 `mappend` $3 }
  | top_decls ';' private_decls { $1 `mappend` $3 }
  | top_decl                    { $1 }
  | public_decls                { $1 }
  | private_decls               { $1 }

top_decl :: { PDecls }
  : data         { $1 }
  | primitive    { $1 }
  | open         { mkOpen $1 }
  | top_fun_bind { mkUntyped $1 }
  | type_bind    { $1 }


-- Primitive Declarations ------------------------------------------------------

primitive :: { PDecls }
  : 'primitive' primitive_body { $2 }

primitive_body :: { PDecls }
  : 'type' tycon '::' kind      { mkPrimType (PrimType $2 $4) }
  | IDENT        '::' qual_type { mkPrimTerm (PrimTerm $1 $3) }


-- Function Declarations -------------------------------------------------------

public_decls :: { PDecls }
  : 'public' '{' binds '}' { exportBlock Public $3 }

private_decls :: { PDecls }
  : 'private' '{' binds '}' { exportBlock Private $3 }

binds :: { PDecls }
  : binds ';' fun_bind  { addDecl $3 $1 }
  | binds ';' type_bind { $3 `mappend` $1 }
  | fun_bind            { mkUntyped $1 }
  | type_bind           { $1 }

qual_name_prefix :: { [Name] }
  : qual_name_prefix '.' CONIDENT { $3:$1 }
  | CONIDENT                      { [$1] }

-- By default, everything is set to be exported as public.
top_fun_bind :: { UntypedDecl }
  : 'public'  fun_bind { $2 { untypedExport = Public } }
  | 'private' fun_bind { $2 { untypedExport = Private } }
  |           fun_bind { $1 { untypedExport = Public } }

type_bind :: { PDecls }
  : IDENT '::' qual_type { mkTypeDecl $1 $3 }

fun_bind :: { UntypedDecl }
  : IDENT arg_list '=' exp { UntypedDecl Private $1 ($2 (MTerm $4)) }

arg_list :: { Match -> Match }
  : arg_list pat { $1 . MPat $2 }
  | {- empty -}  { id }


-- Module Imports --------------------------------------------------------------

open :: { Open }
  : 'open' mod_name open_body { $3 $2 }

open_body :: { QualName -> Open }
  : 'as' mod_name open_tail { \qn -> uncurry (Open qn (Just $2)) $3 }
  | open_tail               { \qn -> uncurry (Open qn Nothing)   $1 }

open_tail :: { (Bool,[OpenSymbol]) }
  : {- empty -}                { (True,  []) }
  | '(' open_list ')'          { (False, $2) }
  | 'hiding' '(' open_list ')' { (True,  $3) }

open_list :: { [OpenSymbol] }
  : open_sym               { [$1] }
  | open_list ',' open_sym { $3:$1 }

open_sym :: { OpenSymbol }
  : IDENT                   { OpenTerm $1 }
  | CONIDENT open_type_tail { OpenType $1 $2 }

open_type_tail :: { [Name] }
  : {- empty -}               { [] }
  | '(' open_type_members ')' { reverse $2 }

open_type_members :: { [Name] }
  : CONIDENT                       { [$1] }
  | open_type_members ',' CONIDENT { $3 : $1 }


-- Data Declarations -----------------------------------------------------------

data :: { PDecls }
  : 'data' '{' constr_groups '}' {% mkDataDecl $2 (reverse $3) }

constr_groups :: { [(Name,Forall ConstrGroup)] }
  : constr_groups ';' constr_group { $3 : $1 }
  | constr_group                   { [$1] }

constr_group :: { (Name,Forall ConstrGroup) }
  : CONIDENT constr_group_body { ($1,$2) }

constr_group_body :: { Forall ConstrGroup }
  : atypes constr_group_tail { $2 $1 }
  |        constr_group_tail { $1 [] }

constr_group_tail :: { [Type] -> Forall ConstrGroup }
  : '=' '{' constrs '}' { \tys -> mkConstrGroup tys (reverse $3) }
  | {- empty -}         { \tys -> mkConstrGroup tys [] }

constrs :: { [Constr] }
  : constrs '|' constr { $3 : $1 }
  | constr             { [$1] }

constr :: { Constr }
  : CONIDENT constr_tail
    { Constr { constrName = $1, constrExport = Public, constrFields = $2 } }

constr_tail :: { [Type] }
  : atypes      { reverse $1 }
  | {- empty -} { [] }


-- Terms -----------------------------------------------------------------------

exp :: { Term }
  : '\\' abs_args '->' lexp { Abs ($2 (MTerm $4)) }
  | lexp                    { $1 }

abs_args :: { Match -> Match  }
  : abs_args pat { $1 . MPat $2 }
  | pat          { MPat $1  }

pat :: { Pat }
  : IDENT { PVar $1 }

lexp :: { Term }
  : 'let' '{' binds '}' 'in' fexp {% (\(ts,us) -> Let ts us $6) `fmap`
                                      processBindings (parsedPDecls $3) }
  | fexp                          { $1 }

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
  : apptype type_tail { $2 (foldl1 (flip tapp) $1) }

type_tail :: { Type -> Type }
  : {- empty -} { id }
  | '->' type   { \a -> tarrow a $2 }

apptype :: { [Type] }
  : apptype atype { $2 : $1 }
  | atype         { [$1] }

atype :: { Type }
  : IDENT        { uvar (TParam 0 True $1 setSort) }
  | CONIDENT     { TCon (simpleName $1) }
  | '(' type ')' { $2 }

atypes :: { [Type] }
  : atypes atype { $2 : $1 }
  | atype        { [$1] }

-- XXX fix the type parameters
qual_type :: { Forall Type }
  : type { mkForall $1 }

tycon :: { String }
  : CONIDENT     { $1 }
  | '(' OPER ')' { $2 }
  -- XXX does reserved syntax really need to be repeated here?
  | '(' '->' ')' { "->" }


-- Kinds -----------------------------------------------------------------------

kind :: { Kind }
  : akind kind_body { $2 $1 }

kind_body :: { Kind -> Kind }
  : {- empty -} { id }
  | '->' kind   { \a -> karrow a $2 }

akind :: { Kind }
  : '*'          { kstar }
  | '(' kind ')' { $2 }


-- Parser Stuff ----------------------------------------------------------------

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = do
  ps <- get
  case psTokens ps of

    l:ls -> do
      set $! ps { psTokens = ls }
      k l

    [l] -> parseError l

    [] -> happyError

happyError :: Parser a
happyError  = raiseP "Happy error" nullPosition

parseError l = raiseP ("Parse error near: " ++ show (lexToken l)) (lexPos l)
}
