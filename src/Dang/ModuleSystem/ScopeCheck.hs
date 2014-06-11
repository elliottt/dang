{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dang.ModuleSystem.ScopeCheck where

import Dang.ModuleSystem.Interface
import Dang.ModuleSystem.QualName
import Dang.Monad
import Dang.Syntax.AST
import Dang.Utils.Location

import           Control.Applicative ( Applicative )
import           Data.Generics ( Data, gmapM, extM )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import           MonadLib ( BaseM(..), runM, Id, ReaderT, ask, local )

-- | Fully-qualify all names in the module, according to their imports.
scopeCheckModule :: Module -> Dang Module
scopeCheckModule m = runScope (scModule m)


-- Scope Checking Monad --------------------------------------------------------

newtype Scope a = Scope { unScope :: ReaderT Names Dang a
                        } deriving (Functor,Applicative,Monad)

instance BaseM Scope Dang where
  inBase m = Scope (inBase m)

runScope :: Scope a -> Dang a
runScope m = runM (unScope m) mempty


-- Renaming Environments -------------------------------------------------------

data NameDef = FromDef (Located QualName)
               -- ^ The location of the definition
             | FromIface (Located Open) QualName
               -- ^ The location of the import responsible
               deriving (Show)


newtype Names = Names { nDefs :: Map.Map QualName [NameDef] }

instance Monoid Names where
  {-# INLINE mempty #-}
  mempty      = Names { nDefs = Map.empty }

  {-# INLINE mappend #-}
  mappend l r = mconcat [l,r]

  {-# INLINE mconcat #-}
  mconcat ns  = Names { nDefs = Map.unionsWith (++) (map nDefs ns) }


-- | Combine two naming environments, where the names from the left shadow the
-- ones on the right.
shadowing :: Names -> Names -> Names
l `shadowing` r = Names { nDefs = Map.union (nDefs l) (nDefs r) }


fromDef :: QualName -> Located QualName -> Names
fromDef n ln = Names { nDefs = Map.singleton n [FromDef ln] }

fromImport :: QualName -> Located Open -> QualName -> Names
fromImport n lo qn = Names { nDefs = Map.singleton n [FromIface lo qn] }


-- Scope Checking --------------------------------------------------------------

-- | Generic traversal.
scPass :: Data a => a -> Scope a
scPass  = gmapM scPass

-- | Rename a module.
scModule :: Module -> Scope Module
scModule  = scPass
