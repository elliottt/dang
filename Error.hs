{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Error where

import Data.Typeable (Typeable,cast)
import MonadLib


data SomeError = forall e. Typeable e => SomeError e

class Typeable e => Error e where
  toSomeError :: e -> SomeError
  toSomeError  = SomeError

  fromSomeError :: SomeError -> Maybe e
  fromSomeError (SomeError e) = cast e

raiseError :: (ExceptionM m SomeError, Error e) => e -> m a
raiseError  = raise . toSomeError

catchError :: (RunExceptionM m SomeError, Error e) => m a -> (e -> m a) -> m a
catchError m k = do
  e <- try m
  case e of
    Right a -> return a
    Left se ->
      case fromSomeError se of
        Nothing -> raise se
        Just e  -> k e
