{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Monad (
    Dang
  , runDang

  , raiseE
  , catchE
  , catchJustE

  , Exception(..)
  , SomeException()
  ) where

import Control.Applicative (Applicative)
import Control.Exception (Exception(..),SomeException)
import Data.Typeable (Typeable)
import MonadLib
import qualified Control.Exception as E

data DangError = DangError String
    deriving (Show,Typeable)

instance Exception DangError


newtype Dang a = Dang
  { getDang :: IO a
  } deriving (Functor,Applicative)

instance Monad Dang where
  {-# INLINE return #-}
  return x = Dang (return x)

  {-# INLINE (>>=) #-}
  Dang m >>= f = Dang (getDang . f =<< m)

  {-# INLINE fail #-}
  fail msg = raiseE (DangError msg)


instance ExceptionM Dang SomeException where
  {-# INLINE raise #-}
  raise = Dang . E.throwIO

instance RunExceptionM Dang SomeException where
  {-# INLINE try #-}
  try = Dang . E.try . getDang

instance BaseM Dang IO where
  {-# INLINE inBase #-}
  inBase = Dang

-- | Turn a Dang operation into an IO operation, by re-throwing any exceptions
-- in IO.
runDang :: Dang a -> IO a
runDang  = getDang

-- | Raise an exception.
raiseE :: (ExceptionM m SomeException, Exception e) => e -> m a
raiseE  = raise . toException

-- | Catch an exception.
catchE :: (RunExceptionM m SomeException, Exception e)
       => m a -> (e -> m a) -> m a
catchE m h = do
  e <- try m
  case e of
    Left se -> maybe (raise se) h (fromException se)
    Right a -> return a

-- | Catch an exception guarded by a predicate.
catchJustE :: (RunExceptionM m SomeException, Exception e)
           => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJustE p m h = catchE m $ \ e ->
  case p =<< fromException e of
    Just b  -> h b
    Nothing -> raiseE e
