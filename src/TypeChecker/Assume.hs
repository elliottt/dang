{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeChecker.Assume where

import Dang.Monad
import QualName

import Data.Typeable (Typeable)
import MonadLib


data Assump t = Assume QualName t
    deriving (Eq,Ord,Show)

prop :: Assump t -> t
prop (Assume _ t) = t

type Assumps t = [Assump t]

data AssumpError = UnboundIdentifier QualName
    deriving (Show,Typeable)

instance Exception AssumpError

noAssumps :: Assumps t
noAssumps  = []

findAssump :: ExceptionM m SomeException => QualName -> Assumps t -> m t
findAssump n []                           = raiseE (UnboundIdentifier n)
findAssump n (Assume n' t:as) | n == n'   = return t
                              | otherwise = findAssump n as

addAssump :: Assump t -> Assumps t -> Assumps t
addAssump  = (:)

-- | This is very wrong, as it doesn't deal with assumption clashes.  Best move
-- to a Map, I think.
mergeAssumps :: Assumps t -> Assumps t -> Assumps t
mergeAssumps  = (++)
