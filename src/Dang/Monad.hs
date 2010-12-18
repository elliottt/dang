{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Monad where

import Error

import Control.Applicative (Applicative)
import Data.Typeable (Typeable)
import MonadLib

data DangError = DangError String
    deriving (Show,Typeable)

instance Error DangError

newtype Dang a = Dang
  { getDang :: ExceptionT SomeError IO a
  } deriving (Functor,Applicative,Monad)

instance ExceptionM Dang SomeError where
  {-# INLINE raise #-}
  raise = Dang . raise

instance RunExceptionM Dang SomeError where
  {-# INLINE try #-}
  try = Dang . try . getDang

instance BaseM Dang IO where
  {-# INLINE inBase #-}
  inBase = Dang . inBase

-- | Turn a Dang operation into an IO operation, by re-throwing any exceptions
-- in IO.
runDang :: Dang a -> IO a
runDang (Dang m) = do
  e <- runExceptionT m
  case e of
    Left se -> fail (show se)
    Right a -> return a

-- Utilities -------------------------------------------------------------------

raiseDang :: String -> Dang a
raiseDang  = raiseE . DangError
