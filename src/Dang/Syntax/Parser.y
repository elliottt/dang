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
  '->' { Located $$ (TKeyword Krarrow) }
  '=>' { Located $$ (TKeyword KRarrow) }

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

  '*'         { Located $$ (TOperIdent "*")       }

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

ident :: { Located String }
  : IDENT { fmap fromTIdent $1 }
  | 'as'  { "as" `at` $1 }


cident :: { Located String }
  : CONIDENT { fmap fromTConIdent $1 }


mod_name :: { Located ModName }
  : sep1('.', cident) { map unLoc $1 `at` getLoc $1 }


qual_cident :: { Located (ModName,String) }
  : sep1('.', cident)
    { case $1 of
        [x] -> ([],) `fmap` x
        xs  -> (map unLoc (init xs), unLoc (last xs)) `at` getLoc $1 }


qual_ident :: { Located (ModName,String) }
  : mod_name '.' ident
    { (unLoc $1, unLoc $3) `at` mconcat [getLoc $1,getLoc $3] }
  | ident
    { fmap ([],) $1 }


-- Modules ---------------------------------------------------------------------

top_module :: { Module }
  : 'module' mod_name 'where' top_decls
    { Module { modName  = $2
             , modDecls = $4 } }


-- Declarations ----------------------------------------------------------------

top_decls :: { [TopDecl] }
  : layout(top_decl) { $1 }

top_decl :: { TopDecl }
  : decl                { TDDecl          $1 }
  | data_decl           { TDData          $1 }
  | prim_type           { TDPrimType      $1 }
  | prim_term           { TDPrimTerm      $1 }
  | local_decls         { TDLocal         $1 }
  | 'public'  top_decls
    { TDExport (Exported Public $2 `at` mappend $1 (getLoc $2)) }
  | 'private' top_decls
    { TDExport (Exported Private $2 `at` mappend $1 (getLoc $2)) }

local_decls :: { Located LocalDecls }
  : 'local' decls 'in' top_decls
    { LocalDecls { ldLocals = $2
                 , ldDecls  = $4
                 } `at` mconcat [$1,$3,getLoc $4] }


decls :: { [Decl] }
  : layout(decl) { $1 }

decl :: { Decl }
  : open      { DOpen $1 }
  | signature { DSig  $1 }
  | bind      { DBind $1 }


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

-- Kinds -----------------------------------------------------------------------

prim_type :: { Located PrimType }
  : 'primitive' cident ':' kind
    { PrimType { primTypeName = mkLocal (Type 0) (unLoc $2)
               , primTypeKind = $4
               } `at` mconcat [$1, $3, getLoc $4] }

kind :: { Kind }
  : sep1('->', akind)
    { TLoc (foldr1 TFun $1 `at` getLoc $1) }

akind :: { Kind }
  : '*'          { TLoc (TCon (mkLocal (Type 1) "*") `at` $1) }
  | qual_cident  { TLoc ((TCon . mkName (Type 1)) `fmap` $1)  }
  | '(' kind ')' { TLoc ($2 `at` mconcat [$1,$3])             }


-- Types -----------------------------------------------------------------------

signature :: { Located Signature }
  : sep1(',', ident) ':' type_schema
    { Signature { sigNames  = fmap (fmap (mkLocal Expr)) $1
                , sigSchema = $3 } `at` mconcat [ getLoc $1
                                                , $2
                                                , getLoc $3 ] }

type_schema :: { Schema }
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
  : ident
    { TLoc (fmap (TVar . mkLocal (Type 0)) $1) }

  | qual_cident
    { TLoc (fmap (TCon . mkTyCon) $1) }

  | '(' sep(',', type) ')'
    { TLoc (mkTuple $2 `at` mappend (getLoc $1) (getLoc $3)) }

  | row
    { $1 }

row :: { Type }
  : '{' sep(',', ltype) opt(row_ext) '}'
    { TLoc (mkTRow $2 $3 `at` mappend (getLoc $1) (getLoc $3)) }

ltype :: { Labelled Type }
  : ident ':' type
    { Labelled { labName  = fmap (mkLocal (Type 0)) $1
               , labValue = $3 } }

row_ext :: { Type }
  : '|' type { $2 }



-- Expressions -----------------------------------------------------------------

prim_term :: { Located PrimTerm }
  : 'primitive' ident ':' type_schema
    { PrimTerm { primTermName = mkLocal Expr (unLoc $2)
               , primTermType = $4
               } `at` mconcat [$1,$3,getLoc $4] }

bind :: { Located Bind }
  : ident list(apat) '=' expr
    { Bind { bindName = mkLocal Expr `fmap` $1
           , bindType = Nothing
           , bindBody = matchPats $2 (MSuccess $4)
           } `at` mappend (getLoc $1) (getLoc $2) }

expr :: { Expr }
  : 'let' decls 'in' expr
    { ELoc (Let $2 $4 `at` mconcat [$1,$3,getLoc $4]) }

  | 'case' expr 'of' case_arms
    { ELoc (Case $2 $4 `at` mconcat [$1,$3,getLoc $4]) }

  | '\\' case_arms
    { ELoc (Abs $2 `at` mconcat [$1,getLoc $2]) }

  | app_expr
    { $1 }

app_expr :: { Expr }
  : list1(aexpr)
    { mkApp $1 }

aexpr :: { Expr }
  : evar
    { ELoc $1 }

  | literal
    { ELoc (Lit `fmap` $1) }

  | '(' expr ')'
    { ELoc ($2 `at` mappend $1 $3) }

evar :: { Located Expr }
  : ident
    { fmap (Var . mkLocal Expr) $1 }

  | sep_body('.',cident) opt(evar_tail)
    { case $2 of
        Just i  -> Var (mkName Expr (reverse (map unLoc $1),unLoc i))
                     `at` mconcat [getLoc $1, getLoc i]
        Nothing -> let (con:pfx) = $1
                    in Con (mkName Expr (reverse (map unLoc pfx),unLoc con))
                         `at` getLoc $1 }

evar_tail :: { Located String }
  : '.' ident { $2 }

case_arms :: { Match }
  : layout1(case_arm) { foldr MSplit MFail $1 }

case_arm :: { Match }
  : pat '->' expr
    { MLoc (MPat $1 (MSuccess $3) `at` mconcat [getLoc $1,$2,getLoc $3]) }

literal :: { Located Literal }
  : INT
    { let TInt i b = unLoc $1
       in (LInt i b `at` getLoc $1) }


-- Patterns --------------------------------------------------------------------

pat :: { Pat }
  : qual_cident list(apat)
    { PLoc (PCon (mkName Expr (unLoc $1)) $2
        `at` mconcat [getLoc $1, getLoc $2]) }

  | apat
    { $1 }

apat :: { Pat }
  : ident
    { PLoc (fmap (PVar . mkLocal Expr) $1) }

  | '_'
    { PLoc (PWildcard `at` $1) }

  | '(' pat ')'
    { PLoc ($2 `at` mconcat [$1,$3]) }


-- Data ------------------------------------------------------------------------

data_decl :: { Located DataDecl }
  : 'data' layout1(constr_group)
    {% mkData $1 $2 }

constr_group :: { (Name, Located ConstrGroup) }
  : cident list(atype) mb_constrs
    { ( mkName (Type 0) ([], unLoc $1)
      , ConstrGroup { groupResTys  = $2
                    , groupConstrs = $3
                    } `at` mconcat [getLoc $1,getLoc $3]) }

mb_constrs :: { [Located Constr] }
  : '=' sep1('|', constr) { $2 }
  | {- empty -}           { [] }

constr :: { Located Constr }
  : cident list(atype)
    { Constr { constrName   = mkDataCon ([],unLoc $1)
             , constrFields = $2
             } `at` mconcat [getLoc $1, getLoc $2] }


-- Combinators -----------------------------------------------------------------

layout(e)
  : '{'  sep(';', e)  '}'  { $2 }
  | 'v{' sep('v;', e) 'v}' { $2 }

layout1(e)
  : '{'  sep1(';', e)  '}'  { $2 }
  | 'v{' sep1('v;', e) 'v}' { $2 }


opt(p)
  : p           { Just $1 }
  | {- empty -} { Nothing }


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
