-- vim: ft=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.Text.Lazy as L
import           Text.Location.Layout (Layout(..),layout)

}


%tokentype { SrcLoc Token }

%token
  QUAL_CON { $$ @ Located { locValue = TQualCon _ _    } }
  CON      { $$ @ Located { locValue = TUnqualCon _    } }
  UNQUAL   { $$ @ Located { locValue = TUnqualIdent _  } }
  QUAL     { $$ @ Located { locValue = TQualIdent _ _  } }
  NUM      { $$ @ Located { locValue = TNum _ _        } }

  'functor'{ Located $$ (TKeyword Kfunctor)}
  'sig'    { Located $$ (TKeyword Ksig)    }
  'struct' { Located $$ (TKeyword Kstruct) }
  'module' { Located $$ (TKeyword Kmodule) }
  'where'  { Located $$ (TKeyword Kwhere)  }
  'type'   { Located $$ (TKeyword Ktype)   }

  'require'{ Located $$ (TKeyword Krequire)}
  'open'   { Located $$ (TKeyword Kopen)   }
  'forall' { Located $$ (TKeyword Kforall) }

  '|'      { Located $$ (TKeyword Kpipe)   }

  '.'      { Located $$ (TKeyword Kdot)    }
  ','      { Located $$ (TKeyword Kcomma)  }

  ':'      { Located $$ (TKeyword Kcolon)  }
  '='      { Located $$ (TKeyword Kassign) }

  'let'    { Located $$ (TKeyword Klet)    }
  'in'     { Located $$ (TKeyword Kin)     }

  '->'     { Located $$ (TKeyword Krarrow) }

  '_'      { Located $$ (TKeyword Kwild)   }

  '('      { Located $$ (TKeyword Klparen) }
  ')'      { Located $$ (TKeyword Krparen) }

  'v{'     { Located $$ TStart }
  'v;'     { Located $$ TSep   }
  'v}'     { Located $$ TEnd   }


%monad { Dang }
%error { parseError }

%name top_module

%%


-- Top-level Module ------------------------------------------------------------

top_module :: { Module Parsed }
  : 'module' mod_name 'where' 'v{' top_decls 'v}'
    { Module { modMeta     = mappend $1 $6
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
  : 'require'        mod_name { Require (getLoc ($1,$2)) $2 False }
  | 'require' 'open' mod_name { Require (getLoc ($1,$3)) $3 True  }


-- Declarations ----------------------------------------------------------------

decl :: { [Decl Parsed] }
  : signature     { [ DSig (getLoc sig) sig | sig <- $1 ] }
  | bind          { [DBind (getLoc $1) $1] }
  | data_decl     { [DData (getLoc $1) $1] }
  | mod_bind      { [$1] }
  | mod_type_bind { [$1] }


-- Module Types ----------------------------------------------------------------

mod_type_bind :: { Decl Parsed }
  : 'module' 'type' con '=' mod_type
    { DModType (getLoc ($1,$5)) $3 $5 }

mod_type :: { ModType Parsed }
  : con
    { MTVar (getLoc $1) $1 }

  | 'sig' 'v{' sep1('v;', mod_spec) 'v}'
    { MTSig (getLoc ($1,$4)) (concat $3) }

  | 'functor' list1(mod_param) '->' mod_type
    { let { mk (p,ty) r = MTFunctor (getLoc (p,r)) p ty r
          } in foldr mk $4 $2 }

mod_spec :: { [ModSpec Parsed] }
  : signature
    { [ MSSig (getLoc sig) sig | sig <- $1 ] }

  | data_decl
    { [MSData (getLoc $1) $1] }

  | 'module' mod_name ':' mod_type
    { [MSMod (getLoc ($1,$4)) $2 $4] }


-- Module Expressions ----------------------------------------------------------

mod_bind :: { Decl Parsed }
  : 'module' mod_name list(mod_param) opt(mod_restrict) '=' mod_expr
    { DModBind (getLoc ($1,$6)) $2 (mkFunctor $3 (restrictMod $4 $6)) }

mod_param :: { (IdentOf Parsed, ModType Parsed) }
  : '(' con ':' mod_type ')'
    { ($2,$4) }

mod_restrict :: { ModType Parsed }
  : ':' mod_type { $2 }

mod_expr :: { ModExpr Parsed }
  : mod_bexpr opt(mod_constraint)
    { case $2 of
        Nothing -> $1
        Just ty -> MEConstraint (getLoc ($1,ty)) $1 ty }

mod_constraint :: { ModType Parsed }
  : ':' mod_type { $2 }

mod_bexpr :: { ModExpr Parsed }
  : list1(mod_aexpr)
    { foldl1 (\ e x -> MEApp (getLoc (e,x)) e x) $1 }

  | mod_struct
    { MEStruct (getLoc $1) $1 }

mod_aexpr :: { ModExpr Parsed }
  : con              { MEName (getLoc $1) $1 }
  | qual_con         { MEName (getLoc $1) $1 }
  | '(' mod_expr ')' { $2                    }

mod_struct :: { ModStruct Parsed }
  : 'struct' 'v{' sep('v;', decl) 'v}'
    { ModStruct (getLoc ($1,$4)) (concat $3) }


-- Types -----------------------------------------------------------------------

signature :: { [Sig Parsed] }
  : sep1(',', ident) ':' schema
    { let { schemaLoc = getLoc $3
          } in [ Sig { sigMeta   = getLoc sig `mappend` schemaLoc
                     , sigName   = sig
                     , sigSchema = $3
                     } | sig <- $1 ] }

schema :: { Schema Parsed }
  : 'forall' list1(ident) '.' type { Schema (mappend $1 (getLoc $4)) $2 $4 }
  |                           type { Schema (getLoc  $1)             [] $1 }

type :: { Type Parsed }
  : sep1('->', app_type) { mkTFun $1 }

app_type :: { Type Parsed }
  : list1(atype) { mkTApp $1 }

atype :: { Type Parsed }
  : ident        { TVar (getLoc $1) $1 }
  | con          { TCon (getLoc $1) $1 }
  | qual_con     { TCon (getLoc $1) $1 }
  | '(' type ')' { $2                  }


-- Expressions -----------------------------------------------------------------

bind :: { Bind Parsed }
  : ident list(pat) '=' expr
    { Bind { bMeta   = getLoc ($1,$4)
           , bName   = $1
           , bParams = $2
           , bBody   = $4 } }

pat :: { Pat Parsed }
  : '_'                    { PWild  $1                     }
  | ident                  { PVar   (getLoc $1) $1         }
  | con                    { PCon   (getLoc $1) $1 []      }
  | '(' con list1(pat) ')' { PCon   (getLoc ($1,$4)) $2 $3 }

expr :: { Expr Parsed }
  : list1(aexpr)
    { mkEApp $1 }

  | 'let' 'v{' sep1('v;', let_decl) 'v}' 'in' expr
    { ELet (getLoc ($1,$6)) (concat $3) $6 }

let_decl :: { [LetDecl Parsed] }
  : bind      { [LDBind (getLoc $1) $1] }
  | signature { [LDSig (getLoc sig) sig | sig <- $1 ] }

aexpr :: { Expr Parsed }
  : ident        { EVar (getLoc $1) $1 }
  | qual_ident   { EVar (getLoc $1) $1 }
  | con          { ECon (getLoc $1) $1 }
  | qual_con     { ECon (getLoc $1) $1 }
  | lit          { ELit (getLoc $1) $1 }
  | '(' expr ')' { $2                  }

lit :: { Literal Parsed }
  : NUM { case thing $1 of TNum base n -> LInt (getLoc $1) base n }


-- Data Declarations -----------------------------------------------------------

data_decl :: { Data Parsed }
  : 'type' con list(ident) opt(data_constrs)
    { Data { dMeta = getLoc ($1, $2, $3, $4)
           , dName = $2
           , dParams = $3
           , dConstrs = fromMaybe [] $4 } }

data_constrs :: { [Constr Parsed] }
  : '=' sep1('|', data_constr) { $2 }

data_constr :: { Constr Parsed }
  : con list(atype)
    { Constr { cMeta = getLoc ($1,$2)
             , cName = $1
             , cParams = $2 } }


-- Names -----------------------------------------------------------------------

mod_name :: { IdentOf Parsed }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }
  | CON      { case thing $1 of TUnqualCon n  -> PUnqual n  <$ $1 }

con :: { IdentOf Parsed }
  : CON { case thing $1 of TUnqualCon n -> PUnqual n <$ $1 }

qual_con :: { IdentOf Parsed }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }

qual_ident :: { IdentOf Parsed }
  : QUAL { case thing $1 of TQualIdent ns n -> PQual ns n <$ $1 }

-- identifiers are unqualified parsed-names
ident :: { IdentOf Parsed }
  : UNQUAL { case thing $1 of TUnqualIdent n -> PUnqual n <$ $1 }


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

lexWithLayout :: Source -> Maybe Position -> L.Text -> [SrcLoc Token]
lexWithLayout src mbStart txt =
  layout Layout { .. } (ignoreComments (lexer src mbStart txt))
  where

  beginsLayout (TKeyword k) = k `elem` [Kwhere, Kstruct, Ksig, Klet]
  beginsLayout _            = False

  endsLayout (TKeyword Kin) = True
  endsLayout _              = False

  start = TStart
  sep   = TSep
  end   = TEnd

parseModule :: Source -> L.Text -> Dang PModule
parseModule src txt = failErrors (top_module (lexWithLayout src Nothing txt))


-- Parser Monad ----------------------------------------------------------------

parseError :: [SrcLoc Token] -> Dang a
parseError toks =
  do case toks of
       loc : _ -> addLoc loc $ \case
         TError _ -> addError ErrLexer  (text "Lexical error")
         _        -> addError ErrParser (text "Parse error")

       [] -> addError ErrParser (text "Unexpected end-of-file")

     mzero


-- Utilities -------------------------------------------------------------------

mkTApp :: [Type Parsed] -> Type Parsed
mkTApp [t]    = t
mkTApp (t:ts) = TApp (getLoc (t,ts)) t ts
mkTApp _      = panic (text "mkTApp: empty list")

mkTFun :: [Type Parsed] -> Type Parsed
mkTFun  = foldr1 $ \ty r -> TFun (getLoc (ty,r)) ty r

mkEApp :: [Expr Parsed] -> Expr Parsed
mkEApp [e]    = e
mkEApp (e:es) = EApp (getLoc (e,es)) e es
mkEApp _      = panic (text "mkEApp: empty list")

addParams :: [Pat Parsed] -> Expr Parsed -> Match Parsed
addParams ps e = foldr step (MExpr (getLoc e) e) ps
  where
  step p r = MPat (getLoc (p,r)) p r

mkFunctor :: [(IdentOf Parsed, ModType Parsed)] -> ModExpr Parsed -> ModExpr Parsed
mkFunctor [] e = e
mkFunctor ps e = foldr step e ps
  where
  step (p,ty) r = MEFunctor (getLoc (p,ty,r)) p ty r

restrictMod :: Maybe (ModType Parsed) -> ModExpr Parsed -> ModExpr Parsed
restrictMod Nothing   e = e
restrictMod (Just ty) e = MEConstraint (getLoc (e,ty)) e ty

listLoc :: (LocSource a ~ Source, HasLoc a) => [a] -> SrcRange
listLoc ls = getLoc (head ls)
}
