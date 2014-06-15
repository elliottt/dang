{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.ModuleSystem.Types where

import Dang.Syntax.Lexeme
import Dang.Utils.Location
import Dang.Utils.Pretty
import Dang.Variables

import Control.Applicative ( (<$>) )
import Data.Foldable ( Foldable )
import Data.Generics ( Data, Typeable )
import Data.Serialize ( Serialize )
import Data.Traversable ( Traversable )
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

instance Names a => Names (Exported a) where
  names f Exported { .. } = Exported exSpec <$> names f exValue

instance FreeVars a => FreeVars (Exported a) where
  freeVars Exported { .. } = freeVars exValue

instance BoundVars a => BoundVars (Exported a) where
  boundVars Exported { .. } = boundVars exValue

ppExported :: (a -> PPDoc) -> Exported a -> PPDoc
ppExported ppVal ex = pp (exSpec ex) <+> ppVal (exValue ex)
