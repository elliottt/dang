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

  'import' { Located $$ (TKeyword Kimport) }
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

top_module :: { PModule }
  : 'module' mod_name 'where' 'v{' top_decls 'v}'
    { Module { modName  = $2
             , modDecls = $5 } }

top_decls :: { [Decl PName] } -- { ([Import],[Decl PName]) }
  : {- empty -}      { [] }
  | sep1('v;', decl) { $1 }

-- Declarations ----------------------------------------------------------------

decl :: { Decl PName }
  : signature { DLoc (DSig     `fmap` $1) }
  | bind      { DLoc (DBind    `fmap` $1) }
  | data_decl { DLoc (DData    `fmap` $1) }
  | mod_bind  { DLoc (DModBind `fmap` $1) }


-- Module Types ----------------------------------------------------------------

mod_type :: { ModType PName }
  : con
    { MTLoc (MTVar `fmap` $1) }

  | 'sig' 'v{' sep1('v;', mod_spec) 'v}'
    { MTLoc (MTSig $3 `at` ($1,$4)) }

  | 'functor' list1(mod_param) '->' mod_type
    { MTLoc (foldr (uncurry MTFunctor) $4 $2 `at` ($1,$4)) }

mod_spec :: { ModSpec PName }
  : signature     { MSLoc (MSSig         `fmap` $1) }
  | data_decl     { MSLoc (MSData        `fmap` $1) }
  | mod_bind_spec { MSLoc (uncurry MSMod `fmap` $1) }

mod_bind_spec :: { SrcLoc (SrcLoc PName, ModType PName) }
  : 'module' mod_name ':' mod_type { ($2,$4) `at` ($1,$4) }


-- Module Expressions ----------------------------------------------------------

mod_bind :: { SrcLoc (ModBind PName) }
  : 'module' mod_name list(mod_param) opt(mod_restrict) '=' mod_expr
    { ModBind { mbName = $2
              , mbExpr = mkFunctor $3 (restrictMod $4 $6) } `at` ($1,$6) }

mod_param :: { (SrcLoc PName, ModType PName) }
  : '(' con ':' mod_type ')' { ($2,$4) }

mod_restrict :: { ModType PName }
  : ':' mod_type { $2 }

mod_expr :: { ModExpr PName }
  : mod_bexpr opt(mod_constraint)
    { case $2 of
        Nothing -> $1
        Just ty -> MELoc (MEConstraint $1 ty `at` ($1,ty)) }

mod_constraint :: { ModType PName }
  : ':' mod_type { $2 }

mod_bexpr :: { ModExpr PName }
  : list1(mod_aexpr) { MELoc (foldl1 MEApp $1 `at` $1) }
  | mod_struct       { MELoc (MEStruct `fmap` $1)      }

mod_aexpr :: { ModExpr PName }
  : con              { MELoc (MEName `fmap` $1) }
  | qual_con         { MELoc (MEName `fmap` $1) }
  | '(' mod_expr ')' { $2                       }

mod_struct :: { SrcLoc (ModStruct PName) }
  : 'struct' 'v{' sep('v;', decl) 'v}' { ModStruct $3 `at` ($1,$4) }


-- Types -----------------------------------------------------------------------

signature :: { SrcLoc (Sig PName) }
  : sep1(',', ident) ':' schema { Sig $1 $3 `at` ($1,$3) }

schema :: { SrcLoc (Schema PName) }
  : 'forall' list1(ident) '.' type { Schema $2 $4 `at` ($1,$3) }
  |                           type { Schema [] $1 `at`  $1     }

type :: { Type PName }
  : sep1('->', app_type) { mkTFun $1 }

app_type :: { Type PName }
  : list1(atype) { mkTApp $1 }

atype :: { Type PName }
  : ident        { TLoc (TVar `fmap` $1) }
  | con          { TLoc (TCon `fmap` $1) }
  | qual_con     { TLoc (TCon `fmap` $1) }
  | '(' type ')' { $2                    }


-- Expressions -----------------------------------------------------------------

bind :: { SrcLoc (Bind PName) }
  : ident list(pat) '=' expr
    { Bind { bName   = $1
           , bSchema = Nothing
           , bParams = $2
           , bBody   = $4 } `at` ($1,$4) }

pat :: { Pat PName }
  : '_'                    { PLoc (PWild   `at` $1)         }
  | ident                  { PLoc (PVar $1 `at` $1)         }
  | con                    { PLoc (PCon $1 [] `at` $1)      }
  | '(' con list1(pat) ')' { PLoc (PCon $2 $3 `at` ($1,$4)) }

expr :: { Expr PName }
  : list1(aexpr)
    { mkEApp $1 }

  | 'let' 'v{' sep1('v;', let_decl) 'v}' 'in' expr
    { ELoc (ELet $3 $6 `at` ($1,$6)) }

let_decl :: { LetDecl PName }
  : bind      { LDLoc (LDBind `fmap` $1) }
  | signature { LDLoc (LDSig  `fmap` $1) }

aexpr :: { Expr PName }
  : ident        { ELoc (EVar `fmap` $1) }
  | qual_ident   { ELoc (EVar `fmap` $1) }
  | con          { ELoc (ECon `fmap` $1) }
  | qual_con     { ELoc (ECon `fmap` $1) }
  | lit          { ELoc (ELit `fmap` $1) }
  | '(' expr ')' { $2                    }

lit :: { SrcLoc Literal }
  : NUM { case thing $1 of TNum base n -> LInt base n `at` $1 }


-- Data Declarations -----------------------------------------------------------

data_decl :: { SrcLoc (Data PName) }
  : 'type' con list(ident) opt(data_constrs)
    { Data { dName = $2
           , dParams = $3
           , dConstrs = fromMaybe [] $4 } `at` ($1,$2,$3,$4) }

data_constrs :: { [SrcLoc (Constr PName)] }
  : '=' sep1('|', data_constr) { $2 }

data_constr :: { SrcLoc (Constr PName) }
  : con list(atype)
    { Constr { cName = $1
             , cParams = $2 } `at` ($1,$2) }


-- Names -----------------------------------------------------------------------

mod_name :: { SrcLoc PName }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }
  | CON      { case thing $1 of TUnqualCon n  -> PUnqual n  <$ $1 }

con :: { SrcLoc PName }
  : CON { case thing $1 of TUnqualCon n -> PUnqual n <$ $1 }

qual_con :: { SrcLoc PName }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }

qual_ident :: { SrcLoc PName }
  : QUAL { case thing $1 of TQualIdent ns n -> PQual ns n <$ $1 }

-- identifiers are unqualified parsed-names
ident :: { SrcLoc PName }
  : UNQUAL { case thing $1 of TUnqualIdent n -> PUnqual n <$ $1 }


-- Utilities -------------------------------------------------------------------

opt(p)
  : {- empty -} { Nothing }
  | p           { Just $1 }

sep(p,q)
  : {- empty -}   { []         }
  | sep_body(p,q) { reverse $1 }

sep1(p,q)
  : sep_body(p,q) { reverse $1 }

sep_body(p,q)
  : q                 { [$1]    }
  | sep_body(p,q) p q { $3 : $1 }

list(p)
  : {- empty -}  { []         }
  | list_body(p) { reverse $1 }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
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

mkTApp :: [Type PName] -> Type PName
mkTApp [t]    = t
mkTApp (t:ts) = TLoc (TApp t ts `at` (t,ts))
mkTApp _      = panic "parser" (text "mkTApp: empty list")

mkTFun :: [Type PName] -> Type PName
mkTFun ts = TLoc (foldr1 TFun ts `at` ts)

mkEApp :: [Expr PName] -> Expr PName
mkEApp [e]    = e
mkEApp (e:es) = ELoc (EApp e es `at` (e,es))
mkEApp _      = panic "parser" (text "mkEApp: empty list")

addParams :: [Pat PName] -> Expr PName -> Match PName
addParams ps e = foldr MPat (MExpr e) ps

mkFunctor :: [(SrcLoc PName, ModType PName)] -> ModExpr PName -> ModExpr PName
mkFunctor [] e = e
mkFunctor ps e = foldr (uncurry MEFunctor) e ps

restrictMod :: Maybe (ModType PName) -> ModExpr PName -> ModExpr PName
restrictMod Nothing   = id
restrictMod (Just ty) = (`MEConstraint` ty)
}
