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

class FreeVars a => DefinesQualName a where
  definedQualName :: a -> QualName

freeLocals :: FreeVars a => a -> Set.Set Name
freeLocals  = Set.map qualSymbol . Set.filter isSimpleName . freeVars


deriving instance Show a => Show (SCC a)

sccFreeNames :: DefinesName a => Namespace -> [a] -> [SCC a]
sccFreeNames ns as = stronglyConnComp graph
  where
  graph = [ (a, qualName ns (definedName a), Set.toList (freeVars a))
          | a <- as ]

sccFreeQualNames :: DefinesQualName a => [a] -> [SCC a]
sccFreeQualNames as = stronglyConnComp graph
  where
  graph = [ (a, definedQualName a, Set.toList (freeVars a)) | a <- as ]

sccToList :: SCC a -> [a]
sccToList (AcyclicSCC a) = [a]
sccToList (CyclicSCC as) = as
