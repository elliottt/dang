{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Utils.Panic where

import Dang.Utils.PP

import qualified Control.Exception as X
import           Data.Typeable (Typeable)


data Panic = Panic String Doc
             deriving (Show,Typeable)

instance X.Exception Panic


panic :: PP msg => String -> msg -> a
panic modName msg = X.throw (Panic modName (pp msg))
