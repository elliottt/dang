{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Dang.ModuleSystem.Rename (
    Renamed,
    renameModule,
  ) where

import Dang.AST
import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.ModuleSystem.Env
import Dang.ModuleSystem.Name
import Dang.Unique (SupplyM,withSupply)
import Dang.Utils.Ident (Namespace,packNamespaceLazy)
import Dang.Utils.PP
import Dang.Utils.Panic

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import           Data.List (nub,partition)
import           Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),StateT,get,sets_)


data Renamed

type instance IdentOf  Renamed = Name
type instance TypeOf   Renamed = Type Renamed
type instance SchemaOf Renamed = Schema Renamed
type instance MetaOf   Renamed = SrcRange


-- | Rename a top-level module.
renameModule :: HasCallStack => Module Parsed -> Dang (Module Renamed)
renameModule m = rename (rnModule m)


-- Renaming Monad --------------------------------------------------------------

type Names = NameTrie [Name]

data Scope = Scope { scopeName    :: !Name
                   , scopePrefix  :: [L.Text]
                   , scopePublic  :: Names
                   , scopePrivate :: Names
                   }

emptyScope :: Name -> [L.Text] -> Scope
emptyScope scopeName scopePrefix =
  Scope { scopePublic = mempty
        , scopePrivate = mempty, .. }

mergeScope :: Scope -> [Scope] -> [Scope]
mergeScope _ []              = [] -- XXX: is this just an error?
mergeScope s (parent : rest) =
  parent { scopePublic = qualify (scopePrefix s) (scopePublic s)
                         `mappend` scopePublic parent
         } : rest

data RW = RW { rwContext :: [Scope]
             }

pushScope :: Scope -> RW -> RW
pushScope scope rw = rw { rwContext = scope : rwContext rw }

popScope :: RW -> RW
popScope rw =
  case rwContext rw of
    scope : rest -> rw { rwContext = mergeScope scope rest }
    _            -> rw

newtype RN a = RN { unRN :: StateT RW Dang a
                  } deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)
  {-# INLINE inBase #-}

rename :: RN a -> Dang a
rename (RN m) =
  do (a,_) <- runM m RW { rwContext = [] }
     return a

-- | Enter a module, and push a new scope on the context stack.
withScope :: IdentOf Parsed -> (Name -> RN a) -> RN a
withScope lname body =
  do modName <- newMod lname
     RN (sets_ (pushScope (emptyScope modName (pnameNamespace (thing lname)))))
     a <- body modName
     RN (sets_ popScope)
     return a

-- | Introduce a new module name.
newMod :: IdentOf Parsed -> RN Name
newMod lname = undefined


-- Renaming --------------------------------------------------------------------

type Rename f = f Parsed -> RN (f Renamed)

rnModule :: Rename Module
rnModule Module { .. } =
  withLoc modMeta $
  withScope modName $ \ n' ->
    do bs' <- traverse rnDecl modDecls
       return Module { modName  = n'
                     , modMeta  = modMeta
                     , modDecls = bs'
                     }

rnDecl :: Rename Decl
rnDecl (DBind    loc bind)        = withLoc loc (DBind     loc <$> rnBind bind)
rnDecl (DSig     loc sig)         = withLoc loc (DSig      loc <$> rnSig  sig)
rnDecl (DData    loc dta)         = withLoc loc (DData     loc <$> rnData dta)

rnDecl (DModBind loc lname mexpr) = undefined
rnDecl (DModType loc lname mtype) = undefined


rnBind :: Rename Bind
rnBind  = undefined

rnSig :: Rename Sig
rnSig  = undefined

rnData :: Rename Data
rnData  = undefined
