{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Dang.ModuleSystem.Types where

import Dang.Syntax.Lexeme
import Dang.Utils.Location
import Dang.Utils.Pretty

import Data.Data ( Data )
import Data.Foldable ( Foldable )
import Data.Serialize ( Serialize )
import Data.Traversable ( Traversable )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )


-- Export Specifications -------------------------------------------------------

data Export = Public | Private
    deriving (Eq,Show,Ord,Data,Typeable,Generic)

instance Serialize Export

instance Pretty Export where
  ppr Public  = pp Kpublic
  ppr Private = pp Kprivate

data Exported a = Exported { exSpec  :: Export
                           , exValue :: a
                           } deriving (Show,Data,Typeable,Functor,Foldable
                                      ,Traversable)

instance HasLocation a => HasLocation (Exported a) where
  getLoc ex = getLoc (exValue ex)
  stripLoc  = fmap stripLoc

ppExported :: (a -> PPDoc) -> Exported a -> PPDoc
ppExported ppVal ex = pp (exSpec ex) <+> ppVal (exValue ex)
