{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Variables where

import QualName

import Data.Graph (SCC(..))
import Data.Graph.SCC (stronglyConnComp)
import qualified Data.Set as Set


class FreeVars a where
  freeVars :: a -> Set.Set QualName

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = maybe Set.empty freeVars

instance FreeVars a => FreeVars [a] where
  freeVars = Set.unions . map freeVars

instance (FreeVars a, FreeVars b) => FreeVars (a,b) where
  freeVars (a,b) = freeVars a `Set.union` freeVars b


class FreeVars a => DefinesName a where
  definedName :: a -> Name

qualDefinedName :: DefinesName a => Namespace -> a -> QualName
qualDefinedName ns = qualName ns . definedName

deriving instance Show a => Show (SCC a)

sccFreeVars :: DefinesName a => Namespace -> [a] -> [SCC a]
sccFreeVars ns as = stronglyConnComp graph
  where
  graph = [ (a, qualDefinedName ns a, Set.toList (freeVars a)) | a <- as ]

sccToList :: SCC a -> [a]
sccToList (AcyclicSCC a) = [a]
sccToList (CyclicSCC as) = as
