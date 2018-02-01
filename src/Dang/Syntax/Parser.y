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
    lexWithLayout,
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
  'data'   { Keyword Kdata    $$ }

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

  'case'   { Keyword Kcase    $$ }
  'of'     { Keyword Kof      $$ }

  '\\'     { Keyword Klambda  $$ }
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
  | type_synonym  { [DSyn  (range $1) $1] }
  | mod_bind      { [$1] }
  | mod_type_bind { [$1] }


-- Module Types ----------------------------------------------------------------

mod_type_bind :: { Decl Parsed }
  : 'module' 'type' con '=' mod_type
    { DModType ($1 <-> $5) $3 $5 }

mod_type :: { ModType Parsed }
  : con
    { MTVar (range $1) $1 }

  | 'sig' layout(mod_spec)
    { let ds = concat $2 in
      MTSig ($1 <-> listLoc ds) ds }

  | 'functor' list1(mod_param) '->' mod_type
    { let { mk (p,ty) r = MTFunctor (p <-> r) p ty r
          } in foldr mk $4 $2 }

mod_spec :: { [ModSpec Parsed] }
  : signature
    { [ MSSig (range sig) sig | sig <- $1 ] }

  | kind_sig
    { [ MSKind (range $1) $1 ] }

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

kind_sig :: { Sig Parsed }
  : 'type' con ':' schema
    { Sig { sigMeta   = $1 <-> $4
          , sigName   = $2
          , sigSchema = $4
          } }

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
  : ident list(arg_pat) '=' expr
    { Bind { bMeta   = $1 <-> $4
           , bName   = $1
           , bSig    = Nothing
           , bParams = $2
           , bBody   = $4 } }

arg_pat :: { Pat Parsed }
  : '_'                       { PWild  $1                }
  | ident                     { PVar   (range $1) $1     }
  | expr_con                  { PCon   (range $1) $1 []  }
  | '(' con list(arg_pat) ')' { PCon   ($1 <-> $4) $2 $3 }

pat :: { Pat Parsed }
  : '_'                    { PWild  $1                           }
  | ident                  { PVar   (range $1) $1                }
  | expr_con list(arg_pat) { PCon   ($1 <-?> listLocMb $2) $1 $2 }

expr :: { Expr Parsed }
  : list1(aexpr)
    { mkEApp $1 }

  | '\\' list1(arg_pat) '->' expr
    { EAbs ($1 <-> $4) $2 $4 }

  | 'let' layout(let_decl) 'in' expr
    { ELet ($1 <-> $4) (concat $2) $4 }

  | 'case' expr 'of' layout(case_arm)
    { ECase ($1 <-> listLoc $4) $2 (mkCases $4) }

let_decl :: { [LetDecl Parsed] }
  : bind      { [LDBind (range $1) $1] }
  | signature { [LDSig (range sig) sig | sig <- $1 ] }

aexpr :: { Expr Parsed }
  : ident        { EVar (range $1) $1 }
  | qual_ident   { EVar (range $1) $1 }
  | expr_con     { ECon (range $1) $1 }
  | lit          { ELit (range $1) $1 }
  | '(' expr ')' { $2                  }

lit :: { Literal Parsed }
  : NUM { case $1 of
            NumLit base val range -> LInt range val base
        }


case_arm :: { Match Parsed }
  : pat '->' expr
    { MPat ($1 <-> $3) $1 (MExpr (range $3) $3) }


-- Data Declarations -----------------------------------------------------------

data_decl :: { Data Parsed }
  : 'data' con list(ident) opt(data_constrs)
    { Data { dMeta = $1 <-> $2 <-?> listLocMb $3 <-?> fmap listLoc $4
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

type_synonym :: { Syn Parsed }
  : 'type' con list(ident) '=' type
    { Syn { synMeta = $1 <-> $5
          , synName = $2
          , synParams = $3
          , synType   = $5 } }


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

-- A constructor that can show up in a pattern or expression.
expr_con :: { IdentOf Parsed }
  : con      { $1 }
  | qual_con { $1 }

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

layout(p) :: { [p] }
  : 'v{' sep1('v;', p) 'v}' { $2 }

opt(p) :: { Maybe p }
  : {- empty -} { Nothing }
  | p           { Just $1 }

sep(p,q) :: { [q] }
  : {- empty -}   { []         }
  | sep_body(p,q) { reverse $1 }

sep1(p,q) :: { [q] }
  : sep_body(p,q) { reverse $1 }

sep_body(p,q) :: { [q] }
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

  beginsLayout (TKeyword k) = k `elem` [Kwhere, Kstruct, Ksig, Klet, Kof]
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

mkTApp :: HasCallStack => [Type Parsed] -> Type Parsed
mkTApp [t]    = t
mkTApp (t:ts) = TApp (t <-> last ts) t ts
mkTApp _      = panic (text "mkTApp: empty list")

mkTFun :: [Type Parsed] -> Type Parsed
mkTFun  = foldr1 $ \ty r -> TFun (ty <-> r) ty r

mkEApp :: HasCallStack => [Expr Parsed] -> Expr Parsed
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

mkCases :: [Match Parsed] -> Match Parsed
mkCases  = foldr1 $ \ m acc -> MSplit (range acc) m acc

restrictMod :: Maybe (ModType Parsed) -> ModExpr Parsed -> ModExpr Parsed
restrictMod Nothing   e = e
restrictMod (Just ty) e = MEConstraint (e <-> ty) e ty

listLoc :: (HasCallStack,HasRange a) => [a] -> SourceRange
listLoc [] = error "listLoc: empty list"
listLoc ls = range (last ls)

listLocMb :: HasRange a => [a] -> Maybe SourceRange
listLocMb [] = Nothing
listLocMb ls = Just (range (last ls))

(<-?>) :: (HasRange a, HasRange b) => a -> Maybe b -> SourceRange
r <-?> Just b = range r <-> b
r <-?> _      = range r
}
