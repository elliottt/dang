module Variables where

import QualName

import qualified Data.Set as Set

class FreeVars a where
  freeVars :: a -> Set.Set QualName

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = maybe Set.empty freeVars

instance FreeVars a => FreeVars [a] where
  freeVars = Set.unions . map freeVars
