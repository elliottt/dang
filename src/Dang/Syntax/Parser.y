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

top_module :: { Module P }
  : 'module' mod_name 'where' 'v{' top_decls 'v}'
    { Module { modMeta  = mappend $1 $6
             , modName  = $2
             , modDecls = $5 } }

top_decls :: { [Decl P] } -- { ([Import],[Decl P]) }
  : {- empty -}      { [] }
  | sep1('v;', decl) { $1 }

-- Declarations ----------------------------------------------------------------

decl :: { Decl P }
  : signature { DSig     (getLoc $1) $1 }
  | bind      { DBind    (getLoc $1) $1 }
  | data_decl { DData    (getLoc $1) $1 }
  | mod_bind  { DModBind (getLoc $1) $1 }


-- Module Types ----------------------------------------------------------------

mod_type :: { ModType P }
  : con
    { MTVar (getLoc $1) $1 }

  | 'sig' 'v{' sep1('v;', mod_spec) 'v}'
    { MTSig (getLoc ($1,$4)) $3 }

  | 'functor' list1(mod_param) '->' mod_type
    { let { mk (p,ty) r = MTFunctor (getLoc (p,r)) p ty r
          } in foldr mk $4 $2 }

mod_spec :: { ModSpec P }
  : signature
    { MSSig (getLoc $1) $1 }

  | data_decl
    { MSData (getLoc $1) $1 }

  | 'module' mod_name ':' mod_type
    { MSMod (getLoc ($1,$4)) $2 $4 }


-- Module Expressions ----------------------------------------------------------

mod_bind :: { ModBind P }
  : 'module' mod_name list(mod_param) opt(mod_restrict) '=' mod_expr
    { ModBind { mbMeta = getLoc ($1,$6)
              , mbName = $2
              , mbExpr = mkFunctor $3 (restrictMod $4 $6) } }

mod_param :: { (IdentOf P, ModType P) }
  : '(' con ':' mod_type ')'
    { ($2,$4) }

mod_restrict :: { ModType P }
  : ':' mod_type { $2 }

mod_expr :: { ModExpr P }
  : mod_bexpr opt(mod_constraint)
    { case $2 of
        Nothing -> $1
        Just ty -> MEConstraint (getLoc ($1,ty)) $1 ty }

mod_constraint :: { ModType P }
  : ':' mod_type { $2 }

mod_bexpr :: { ModExpr P }
  : list1(mod_aexpr)
    { foldl1 (\ e x -> MEApp (getLoc (e,x)) e x) $1 }

  | mod_struct
    { MEStruct (getLoc $1) $1 }

mod_aexpr :: { ModExpr P }
  : con              { MEName (getLoc $1) $1 }
  | qual_con         { MEName (getLoc $1) $1 }
  | '(' mod_expr ')' { $2                    }

mod_struct :: { ModStruct P }
  : 'struct' 'v{' sep('v;', decl) 'v}'
    { ModStruct (getLoc ($1,$4)) $3 }


-- Types -----------------------------------------------------------------------

signature :: { Sig P }
  : sep1(',', ident) ':' schema
    { Sig { sigMeta   = listLoc $1 `mappend` getLoc $3
          , sigNames  = $1
          , sigSchema = $3
          } }

schema :: { Schema P }
  : 'forall' list1(ident) '.' type { Schema (mappend $1 (getLoc $4)) $2 $4 }
  |                           type { Schema (getLoc  $1)             [] $1 }

type :: { Type P }
  : sep1('->', app_type) { mkTFun $1 }

app_type :: { Type P }
  : list1(atype) { mkTApp $1 }

atype :: { Type P }
  : ident        { TVar (getLoc $1) $1 }
  | con          { TCon (getLoc $1) $1 }
  | qual_con     { TCon (getLoc $1) $1 }
  | '(' type ')' { $2                  }


-- Expressions -----------------------------------------------------------------

bind :: { Bind P }
  : ident list(pat) '=' expr
    { Bind { bMeta   = getLoc ($1,$4)
           , bName   = $1
           , bParams = $2
           , bBody   = $4 } }

pat :: { Pat P }
  : '_'                    { PWild  $1                     }
  | ident                  { PVar   (getLoc $1) $1         }
  | con                    { PCon   (getLoc $1) $1 []      }
  | '(' con list1(pat) ')' { PCon   (getLoc ($1,$4)) $2 $3 }

expr :: { Expr P }
  : list1(aexpr)
    { mkEApp $1 }

  | 'let' 'v{' sep1('v;', let_decl) 'v}' 'in' expr
    { ELet (getLoc ($1,$6)) $3 $6 }

let_decl :: { LetDecl P }
  : bind      { LDBind (getLoc $1) $1 }
  | signature { LDSig  (getLoc $1) $1 }

aexpr :: { Expr P }
  : ident        { EVar (getLoc $1) $1 }
  | qual_ident   { EVar (getLoc $1) $1 }
  | con          { ECon (getLoc $1) $1 }
  | qual_con     { ECon (getLoc $1) $1 }
  | lit          { ELit (getLoc $1) $1 }
  | '(' expr ')' { $2                  }

lit :: { Literal P }
  : NUM { case thing $1 of TNum base n -> LInt (getLoc $1) base n }


-- Data Declarations -----------------------------------------------------------

data_decl :: { Data P }
  : 'type' con list(ident) opt(data_constrs)
    { Data { dMeta = getLoc ($1, $2, $3, $4)
           , dName = $2
           , dParams = $3
           , dConstrs = fromMaybe [] $4 } }

data_constrs :: { [Constr P] }
  : '=' sep1('|', data_constr) { $2 }

data_constr :: { Constr P }
  : con list(atype)
    { Constr { cMeta = getLoc ($1,$2)
             , cName = $1
             , cParams = $2 } }


-- Names -----------------------------------------------------------------------

mod_name :: { IdentOf P }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }
  | CON      { case thing $1 of TUnqualCon n  -> PUnqual n  <$ $1 }

con :: { IdentOf P }
  : CON { case thing $1 of TUnqualCon n -> PUnqual n <$ $1 }

qual_con :: { IdentOf P }
  : QUAL_CON { case thing $1 of TQualCon ns n -> PQual ns n <$ $1 }

qual_ident :: { IdentOf P }
  : QUAL { case thing $1 of TQualIdent ns n -> PQual ns n <$ $1 }

-- identifiers are unqualified parsed-names
ident :: { IdentOf P }
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

mkTApp :: [Type P] -> Type P
mkTApp [t]    = t
mkTApp (t:ts) = TApp (getLoc (t,ts)) t ts
mkTApp _      = panic (text "mkTApp: empty list")

mkTFun :: [Type P] -> Type P
mkTFun  = foldr1 $ \ty r -> TFun (getLoc (ty,r)) ty r

mkEApp :: [Expr P] -> Expr P
mkEApp [e]    = e
mkEApp (e:es) = EApp (getLoc (e,es)) e es
mkEApp _      = panic (text "mkEApp: empty list")

addParams :: [Pat P] -> Expr P -> Match P
addParams ps e = foldr step (MExpr (getLoc e) e) ps
  where
  step p r = MPat (getLoc (p,r)) p r

mkFunctor :: [(IdentOf P, ModType P)] -> ModExpr P -> ModExpr P
mkFunctor [] e = e
mkFunctor ps e = foldr step e ps
  where
  step (p,ty) r = MEFunctor (getLoc (p,ty,r)) p ty r

restrictMod :: Maybe (ModType P) -> ModExpr P -> ModExpr P
restrictMod Nothing   e = e
restrictMod (Just ty) e = MEConstraint (getLoc (e,ty)) e ty

listLoc :: (LocSource a ~ Source, HasLoc a) => [a] -> SrcRange
listLoc ls = getLoc (head ls)
}
