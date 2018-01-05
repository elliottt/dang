-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Dang.Syntax.Parser (
    parseModule,
    lexWithLayout
  ) where

import Dang.Monad
import Dang.AST
import Dang.Syntax.AST
import Dang.Syntax.Lexer
import Dang.Syntax.Location
import Dang.Utils.Ident
import Dang.Utils.PP (text)
import Dang.Utils.Panic

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Text.Layout.OffSides (Layout(..),layout,wrapToken)

}


%tokentype { Lexeme Token }

%token
  QUAL_CON { $$ @ Lexeme { lexemeToken = TQualCon _ _    } }
  CON      { $$ @ Lexeme { lexemeToken = TUnqualCon _    } }
  UNQUAL   { $$ @ Lexeme { lexemeToken = TUnqualIdent _  } }
  QUAL     { $$ @ Lexeme { lexemeToken = TQualIdent _ _  } }
  NUM      { $$ @ Lexeme { lexemeToken = TNum _ _        } }

  'functor'{ Keyword Kfunctor $$ }
  'sig'    { Keyword Ksig     $$ }
  'struct' { Keyword Kstruct  $$ }
  'module' { Keyword Kmodule  $$ }
  'where'  { Keyword Kwhere   $$ }
  'type'   { Keyword Ktype    $$ }

  'require'{ Keyword Krequire $$ }
  'open'   { Keyword Kopen    $$ }
  'forall' { Keyword Kforall  $$ }

  '|'      { Keyword Kpipe    $$ }

  '.'      { Keyword Kdot     $$ }
  ','      { Keyword Kcomma   $$ }

  ':'      { Keyword Kcolon   $$ }
  '='      { Keyword Kassign  $$ }

  'let'    { Keyword Klet     $$ }
  'in'     { Keyword Kin      $$ }

  '->'     { Keyword Krarrow  $$ }

  '_'      { Keyword Kwild    $$ }

  '('      { Keyword Klparen  $$ }
  ')'      { Keyword Krparen  $$ }

  'v{'     { Lexeme { lexemeToken = TStart, lexemeRange = $$ } }
  'v;'     { Lexeme { lexemeToken = TSep,   lexemeRange = $$ } }
  'v}'     { Lexeme { lexemeToken = TEnd,   lexemeRange = $$ } }


%monad { Dang }
%error { parseError }

%name top_module

%%


-- Top-level Module ------------------------------------------------------------

top_module :: { Module Parsed }
  : 'module' mod_name 'where' 'v{' top_decls 'v}'
    { Module { modMeta     = $1 <-> $6
             , modName     = $2
             , modRequires = fst $5
             , modDecls    = snd $5 } }

top_decls :: { ([Require Parsed],[Decl Parsed]) }
  : {- empty -}                               { ([], [])        }
  | sep1('v;', require)                       { ($1, [])        }
  | sep1('v;', require) 'v;' sep1('v;', decl) { ($1, concat $3) }
  | sep1('v;', decl)                          { ([], concat $1) }


-- Require Statements ----------------------------------------------------------

require :: { Require Parsed }
  : 'require'        mod_name { Require ($1 <-> $2) $2 False }
  | 'require' 'open' mod_name { Require ($1 <-> $3) $3 True  }


-- Declarations ----------------------------------------------------------------

decl :: { [Decl Parsed] }
  : signature     { [ DSig (range sig) sig | sig <- $1 ] }
  | bind          { [DBind (range $1) $1] }
  | data_decl     { [DData (range $1) $1] }
  | mod_bind      { [$1] }
  | mod_type_bind { [$1] }


-- Module Types ----------------------------------------------------------------

mod_type_bind :: { Decl Parsed }
  : 'module' 'type' con '=' mod_type
    { DModType ($1 <-> $5) $3 $5 }

mod_type :: { ModType Parsed }
  : con
    { MTVar (range $1) $1 }

  | 'sig' 'v{' sep1('v;', mod_spec) 'v}'
    { MTSig ($1 <-> $4) (concat $3) }

  | 'functor' list1(mod_param) '->' mod_type
    { let { mk (p,ty) r = MTFunctor (p <-> r) p ty r
          } in foldr mk $4 $2 }

mod_spec :: { [ModSpec Parsed] }
  : signature
    { [ MSSig (range sig) sig | sig <- $1 ] }

  | data_decl
    { [MSData (range $1) $1] }

  | 'module' mod_name ':' mod_type
    { [MSMod ($1 <-> $4) $2 $4] }


-- Module Expressions ----------------------------------------------------------

mod_bind :: { Decl Parsed }
  : 'module' mod_name list(mod_param) opt(mod_restrict) '=' mod_expr
    { DModBind ($1 <-> $6) $2 (mkFunctor $3 (restrictMod $4 $6)) }

mod_param :: { (SourceRange,IdentOf Parsed, ModType Parsed) }
  : '(' con ':' mod_type ')'
    { ($1 <-> $5, $2,$4) }

mod_restrict :: { ModType Parsed }
  : ':' mod_type { $2 }

mod_expr :: { ModExpr Parsed }
  : mod_bexpr opt(mod_constraint)
    { case $2 of
        Nothing -> $1
        Just ty -> MEConstraint ($1 <-> ty) $1 ty }

mod_constraint :: { ModType Parsed }
  : ':' mod_type { $2 }

mod_bexpr :: { ModExpr Parsed }
  : list1(mod_aexpr)
    { foldl1 (\ e x -> MEApp (e <-> x) e x) $1 }

  | mod_struct
    { MEStruct (range $1) $1 }

mod_aexpr :: { ModExpr Parsed }
  : con              { MEName (range $1) $1 }
  | qual_con         { MEName (range $1) $1 }
  | '(' mod_expr ')' { $2                    }

mod_struct :: { ModStruct Parsed }
  : 'struct' 'v{' sep('v;', decl) 'v}'
    { ModStruct ($1 <-> $4) (concat $3) }


-- Types -----------------------------------------------------------------------

signature :: { [Sig Parsed] }
  : sep1(',', ident) ':' schema
    { let { schemaLoc = range $3
          } in [ Sig { sigMeta   = range sig <-> schemaLoc
                     , sigName   = sig
                     , sigSchema = $3
                     } | sig <- $1 ] }

schema :: { Schema Parsed }
  : 'forall' list1(ident) '.' type { Schema ($1 <-> $4) $2 $4 }
  |                           type { Schema (range  $1) [] $1 }

type :: { Type Parsed }
  : sep1('->', app_type) { mkTFun $1 }

app_type :: { Type Parsed }
  : list1(atype) { mkTApp $1 }

atype :: { Type Parsed }
  : ident        { TVar (range $1) $1 }
  | con          { TCon (range $1) $1 }
  | qual_con     { TCon (range $1) $1 }
  | '(' type ')' { $2                  }


-- Expressions -----------------------------------------------------------------

bind :: { Bind Parsed }
  : ident list(pat) '=' expr
    { Bind { bMeta   = $1 <-> $4
           , bName   = $1
           , bParams = $2
           , bBody   = $4 } }

pat :: { Pat Parsed }
  : '_'                    { PWild  $1                     }
  | ident                  { PVar   (range $1) $1         }
  | con                    { PCon   (range $1) $1 []      }
  | '(' con list1(pat) ')' { PCon   ($1 <-> $4) $2 $3 }

expr :: { Expr Parsed }
  : list1(aexpr)
    { mkEApp $1 }

  | 'let' 'v{' sep1('v;', let_decl) 'v}' 'in' expr
    { ELet ($1 <-> $6) (concat $3) $6 }

let_decl :: { [LetDecl Parsed] }
  : bind      { [LDBind (range $1) $1] }
  | signature { [LDSig (range sig) sig | sig <- $1 ] }

aexpr :: { Expr Parsed }
  : ident        { EVar (range $1) $1 }
  | qual_ident   { EVar (range $1) $1 }
  | con          { ECon (range $1) $1 }
  | qual_con     { ECon (range $1) $1 }
  | lit          { ELit (range $1) $1 }
  | '(' expr ')' { $2                  }

lit :: { Literal Parsed }
  : NUM { case $1 of
            NumLit base val range -> LInt range val base
        }


-- Data Declarations -----------------------------------------------------------

data_decl :: { Data Parsed }
  : 'type' con list(ident) opt(data_constrs)
    { Data { dMeta = $1 <-> $2 <-> listLoc $3 <-?> fmap listLoc $4
           , dName = $2
           , dParams = $3
           , dConstrs = fromMaybe [] $4 } }

data_constrs :: { [Constr Parsed] }
  : '=' sep1('|', data_constr) { $2 }

data_constr :: { Constr Parsed }
  : con list(atype)
    { Constr { cMeta = $1 <-> listRange $2
             , cName = $1
             , cParams = $2 } }


-- Names -----------------------------------------------------------------------

mod_name :: { IdentOf Parsed }
  : QUAL_CON
    { case $1 of
        Lexeme { lexemeToken = TQualCon ns n, .. } -> PQual lexemeRange ns n }

  | CON
    { case $1 of
        Lexeme { lexemeToken = TUnqualCon n, .. }  -> PUnqual lexemeRange n  }

con :: { IdentOf Parsed }
  : CON
    { case $1 of
        Lexeme { lexemeToken = TUnqualCon n, .. } -> PUnqual lexemeRange n }

qual_con :: { IdentOf Parsed }
  : QUAL_CON
    { case $1 of
        Lexeme { lexemeToken = TQualCon ns n, .. } -> PQual lexemeRange ns n }

qual_ident :: { IdentOf Parsed }
  : QUAL
    { case $1 of
        Lexeme { lexemeToken = TQualIdent ns n, .. } -> PQual lexemeRange ns n }

-- identifiers are unqualified parsed-names
ident :: { IdentOf Parsed }
  : UNQUAL
    { case $1 of
        Lexeme { lexemeToken = TUnqualIdent n, .. } -> PUnqual lexemeRange n }


-- Utilities -------------------------------------------------------------------

opt(p) :: { Maybe p }
  : {- empty -} { Nothing }
  | p           { Just $1 }

sep(p,q) :: { [p] }
  : {- empty -}   { []         }
  | sep_body(p,q) { reverse $1 }

sep1(p,q) :: { [p] }
  : sep_body(p,q) { reverse $1 }

sep_body(p,q) :: { [p] }
  : q                 { [$1]    }
  | sep_body(p,q) p q { $3 : $1 }

list(p) :: { [p] }
  : {- empty -}  { []         }
  | list_body(p) { reverse $1 }

list1(p) :: { [p] }
  : list_body(p) { reverse $1 }

list_body(p) :: { [p] }
  : p              { [$1]    }
  | list_body(p) p { $2 : $1 }


-- External Interface ----------------------------------------------------------

{

lexWithLayout :: Source -> Maybe SourcePos -> T.Text -> [Lexeme Token]
lexWithLayout src mbStart txt =
  layout Layout { .. } (ignoreComments (lexer src mbStart txt))
  where

  beginsLayout (TKeyword k) = k `elem` [Kwhere, Kstruct, Ksig, Klet]
  beginsLayout _            = False

  endsLayout (TKeyword Kin) = True
  endsLayout _              = False

  start = wrapToken TStart
  sep   = wrapToken TSep
  end   = wrapToken TEnd

parseModule :: Source -> T.Text -> Dang PModule
parseModule src txt = failErrors (top_module (lexWithLayout src Nothing txt))


-- Parser Monad ----------------------------------------------------------------

parseError :: [Lexeme Token] -> Dang a
parseError toks =
  do case toks of
       loc : _ -> withLoc loc $ case lexemeToken loc of
         TError _ -> addError ErrLexer  (text "Lexical error")
         _        -> addError ErrParser (text "Parse error")

       [] -> addError ErrParser (text "Unexpected end-of-file")

     mzero


-- Utilities -------------------------------------------------------------------

pattern Keyword kw r <- Lexeme { lexemeToken = TKeyword kw, lexemeRange = r }

pattern NumLit base n r <- Lexeme { lexemeToken = TNum base n, lexemeRange = r }

mkTApp :: [Type Parsed] -> Type Parsed
mkTApp [t]    = t
mkTApp (t:ts) = TApp (t <-> last ts) t ts
mkTApp _      = panic (text "mkTApp: empty list")

mkTFun :: [Type Parsed] -> Type Parsed
mkTFun  = foldr1 $ \ty r -> TFun (ty <-> r) ty r

mkEApp :: [Expr Parsed] -> Expr Parsed
mkEApp [e]    = e
mkEApp (e:es) = EApp (e <-> last es) e es
mkEApp _      = panic (text "mkEApp: empty list")

addParams :: [Pat Parsed] -> Expr Parsed -> Match Parsed
addParams ps e = foldr step (MExpr (range e) e) ps
  where
  step p r = MPat (p <-> r) p r

mkFunctor ::
  [(SourceRange,IdentOf Parsed, ModType Parsed)]
  -> ModExpr Parsed
  -> ModExpr Parsed
mkFunctor [] e = e
mkFunctor ps e = foldr step e ps
  where
  step (r,p,ty) body = MEFunctor (r <-> body) p ty body

restrictMod :: Maybe (ModType Parsed) -> ModExpr Parsed -> ModExpr Parsed
restrictMod Nothing   e = e
restrictMod (Just ty) e = MEConstraint (e <-> ty) e ty

listLoc :: HasRange a => [a] -> SourceRange
listLoc ls = range (head ls)

(<-?>) :: HasRange b => SourceRange -> Maybe b -> SourceRange
r <-?> Just b = r <-> b
r <-?> _      = r
}
