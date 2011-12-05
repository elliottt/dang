module ModuleSystem.Types where

import QualName (QualName,simpleName,qualSymbol)


data UsedName
  = UsedType QualName
  | UsedTerm QualName
    deriving (Ord,Show,Eq)

mapUsedName :: (QualName -> QualName) -> (UsedName -> UsedName)
mapUsedName f (UsedType qn) = UsedType (f qn)
mapUsedName f (UsedTerm qn) = UsedTerm (f qn)

simpleUsedName :: UsedName -> UsedName
simpleUsedName  = mapUsedName (simpleName . qualSymbol)

usedQualName :: UsedName -> QualName
usedQualName (UsedType qn) = qn
usedQualName (UsedTerm qn) = qn
