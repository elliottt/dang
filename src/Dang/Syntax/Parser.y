{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE Trustworthy #-}

-- vim: filetype=haskell

module Dang.Syntax.Parser where

import Dang.ModuleSystem.Export
import Dang.ModuleSystem.QualName
import Dang.ModuleSystem.Types
import Dang.Syntax.AST
import Dang.Syntax.Lexeme
import Dang.Syntax.Lexer
import Dang.Syntax.ParserCore
import Dang.TypeChecker.Types
import Dang.TypeChecker.Vars
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
%error { parseError' }

%name parseModule top_module
%name parseTerm   exp
%name parseType   type
%name parseScheme qual_type

%tokentype { Lexeme }

%lexer { lexer } { Located mempty TEof }

%%

-- Names -----------------------------------------------------------------------

qual_name :: { Located QualName }
  : qual_name_prefix '.' ident { let { loc = mappend (getLoc $3) (foldMap getLoc $1)
                                     ; ns  = map unLoc (reverse $1)
                                     } in QualName ns (unLoc $3) `at` loc }
  | ident                      { fmap simpleName $1 }
  | 'as'                       { simpleName "as" `at` $1 }

mod_name :: { Located QualName }
  : qual_name_prefix '.' conident { let { loc = mappend (getLoc $3) (foldMap getLoc $1)
                                        ; ns  = map unLoc (reverse $1)
                                        } in QualName ns (unLoc $3) `at` loc }
  | conident                      { fmap (QualName []) $1 }


-- Modules ---------------------------------------------------------------------

top_module :: { Module }
  : 'module' mod_name 'where' '{' top_decls '}' {% mkModule (unLoc $2) $5 }


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
  : 'type' tycon '::' kind
    { mkPrimType ($1 `mappend` getLoc $2) (PrimType (unLoc $2) $4) }

  | ident        '::' qual_type
    { mkPrimTerm (getLoc $1) (PrimTerm (unLoc $1) $3) }


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

qual_name_prefix :: { [Located Name] }
  : qual_name_prefix '.' conident { $3 : $1 }
  | conident                      { [$1] }

-- By default, everything is set to be exported as public.
top_fun_bind :: { Located UntypedDecl }
  : 'public' fun_bind
    { fmap (\u -> u { untypedExport = Public }) (extendLoc $1 $2) }

  | 'private' fun_bind
    { fmap (\u -> u { untypedExport = Private }) (extendLoc $1 $2) }

  | fun_bind
    { fmap (\u -> u { untypedExport = Public }) $1 }

type_bind :: { PDecls }
  : ident '::' qual_type { mkTypeDecl (getLoc $1) (unLoc $1) $3 }

fun_bind :: { Located UntypedDecl }
  : ident arg_list '=' exp
    { UntypedDecl Private (unLoc $1) ($2 (MTerm (TLoc $4)))
        `at` (getLoc $1 `mappend` getLoc $4)
    }

arg_list :: { Match -> Match }
  : pats        { \z -> foldl (flip MPat) z $1 }
  | {- empty -} { id }


-- Module Imports --------------------------------------------------------------

open :: { Located Open }
  : 'open' mod_name open_body { unLoc $3 (unLoc $2) `at` ($1 `mappend` getLoc $3) }

open_body :: { Located (QualName -> Open) }
  : 'as' mod_name open_tail { (\qn -> uncurry (Open qn (Just (unLoc $2))) (unLoc $3) )
                                 `at` ($1 `mappend` getLoc $3) }
  | open_tail               { (\qn -> uncurry (Open qn Nothing) (unLoc $1) )
                                 `at` getLoc $1 }

open_tail :: { Located (Bool,[OpenSymbol]) }
  : {- empty -}                { (True,  []) `at` mempty            }
  | '(' open_list ')'          { (False, $2) `at` ($1 `mappend` $3) }
  | 'hiding' '(' open_list ')' { (True,  $3) `at` ($1 `mappend` $4) }

open_list :: { [OpenSymbol] }
  : open_sym               { [$1] }
  | open_list ',' open_sym { $3:$1 }

open_sym :: { OpenSymbol }
  : ident                   { OpenTerm (unLoc $1) }
  | conident open_type_tail { OpenType (unLoc $1) $2 }

open_type_tail :: { [Name] }
  : {- empty -}               { [] }
  | '(' open_type_members ')' { reverse $2 }

open_type_members :: { [ Name] }
  : conident                       { [unLoc $1] }
  | open_type_members ',' conident { unLoc $3 : $1 }


-- Data Declarations -----------------------------------------------------------

data :: { PDecls }
  : 'data' '{' constr_groups '}' {% mkDataDecl $2 (reverse $3) }

constr_groups :: { [(Name,Forall ConstrGroup)] }
  : constr_groups ';' constr_group { $3 : $1 }
  | constr_group                   { [$1] }

constr_group :: { (Name,Forall ConstrGroup) }
  : conident constr_group_body { (unLoc $1,$2) }

constr_group_body :: { Forall ConstrGroup }
  : atypes constr_group_tail { $2 (reverse $1) }
  |        constr_group_tail { $1 [] }

constr_group_tail :: { [Type] -> Forall ConstrGroup }
  : '=' '{' constrs '}' { \tys -> mkConstrGroup tys (reverse $3) }
  | {- empty -}         { \tys -> mkConstrGroup tys [] }

constrs :: { [Constr] }
  : constrs '|' constr { $3 : $1 }
  | constr             { [$1] }

constr :: { Constr }
  : conident constr_tail
    { Constr { constrName = unLoc $1, constrExport = Public, constrFields = $2 } }

constr_tail :: { [Type] }
  : atypes      { reverse $1 }
  | {- empty -} { [] }


-- Terms -----------------------------------------------------------------------

exp :: { Located Term }
  : '\\' arg_list '->' lexp
    { let loc = mappend $1 (getLoc $4)
       in Abs ($2 (MTerm (TLoc $4))) `at` loc }
  | lexp
    { $1 }

lexp :: { Located Term }
  : 'let' '{' binds '}' 'in' fexp
    {% do { (ts,us) <- processBindings (parsedPDecls $3)
          ; let loc = mappend $1 (getLoc $6)
          ; return (Let ts us (TLoc $6) `at` loc)
          }

    }
  | fexp
    { $1 }

fexp :: { Located Term }
  : aexp aexp_list
    { let { loc = getLoc $1 `mappend` foldMap getLoc $2
          ; f   = TLoc $1
          ; xs  = map TLoc (reverse $2)
          } in if null xs
                  then $1
                  else apply f xs `at` loc
    }

aexp_list :: { [Located Term] }
  : aexp_list aexp { $2 : $1 }
  | {- empty -}    { [] }

aexp :: { Located Term }
  : '(' exp ')'           { $2 }
  | qual_name             { fmap Global $1 }
  | mod_name              { fmap Global $1 }
  | int                   { fmap (Lit . LInt) $1 }
  | 'case' fexp 'of' '{' case_branches '}'
    { let loc = mappend $1 $6
       in Case (TLoc $2) (foldr MSplit MFail (reverse $5)) `at` loc
    }

case_branches :: { [Match] }
  : case_branches ';' case_branch { $3:$1 }
  | case_branch                   { [$1]  }

case_branch :: { Match }
  : naked_pat '->' exp { MPat $1 (MTerm (TLoc $3)) }


-- Patterns --------------------------------------------------------------------

pat :: { Pat }
  : simple_pat          { $1 }
  | '(' complex_pat ')' { $2 }

pats :: { [Pat] }
  : pats pat { $2 : $1 }
  | pat      { [$1] }

naked_pat :: { Pat }
  : simple_pat  { $1 }
  | complex_pat { $1 }

simple_pat :: { Pat }
  : ident { PVar (unLoc $1) }
  | 'as'  { PVar "as" }
  | '_'   { PWildcard }

complex_pat :: { Pat }
  : conident      { PCon (simpleName (unLoc $1)) [] }
  | conident pats { PCon (simpleName (unLoc $1)) (reverse $2) }


-- Types -----------------------------------------------------------------------

type :: { Type }
  : apptype type_tail { $2 (foldr1 (flip tapp) $1) }

type_tail :: { Type -> Type }
  : {- empty -} { id }
  | '->' type   { (`tarrow` $2) }

apptype :: { [Type] }
  : apptype atype { $2 : $1 }
  | atype         { [$1] }

atype :: { Type }
  : ident        { uvar (TParam 0 True (unLoc $1) setSort) }
  | conident     { TCon (simpleName (unLoc $1)) }
  | '(' type ')' { $2 }

atypes :: { [Type] }
  : atypes atype { $2 : $1 }
  | atype        { [$1] }

-- XXX fix the type parameters
-- XXX add location information
qual_type :: { Scheme }
  : type { mkScheme emptyCxt $1 }

tycon :: { Located String }
  : conident     { $1 }
  | '(' oper ')' { $2 }
  -- XXX does reserved syntax really need to be repeated here?
  | '(' '->' ')' { "->" `at` ($1 `mappend` $3) }


-- Kinds -----------------------------------------------------------------------

-- XXX add location information on this
kind :: { Kind }
  : akind kind_body { $2 $1 }

kind_body :: { Kind -> Kind }
  : {- empty -} { id }
  | '->' kind   { \a -> karrow a $2 }

akind :: { Kind }
  : '*'          { kstar }
  | '(' kind ')' { $2 }


-- Utilities -------------------------------------------------------------------

ident :: { Located String }
  : IDENT { fmap fromSymIdent $1 }

conident :: { Located String }
  : CONIDENT { fmap fromConIdent $1 }

oper :: { Located String }
  : OPER { fmap fromOperIdent $1 }

int :: { Located Integer }
  : INT { fmap fromInt $1 }


-- Parser Stuff ----------------------------------------------------------------

{
lexer :: (Lexeme -> Parser a) -> Parser a
lexer k = do
  ps <- get
  case psTokens ps of

    l:ls -> do
      set $! ps { psTokens = ls }
      k l

    [] -> happyError

happyError :: Parser a
happyError  = parseError mempty (text "Happy error")

parseError' l = parseError (getLoc l) (text "Parse error near:" <+> ppLoc (getLoc l))
}
