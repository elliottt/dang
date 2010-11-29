{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Error where

import Data.Typeable (Typeable,cast)
import MonadLib


data SomeError = forall e. (Show e, Typeable e) => SomeError e

instance Show SomeError where
  showsPrec i (SomeError se) = parens body
    where
    parens b | i > 1     = showChar '(' . b . showChar ')'
             | otherwise = b

    body = showString "SomeError " . showsPrec 1 se


class (Show e, Typeable e) => Error e where
  toSomeError :: e -> SomeError
  toSomeError  = SomeError

  fromSomeError :: SomeError -> Maybe e
  fromSomeError (SomeError e) = cast e

raiseE :: (ExceptionM m SomeError, Error e) => e -> m a
raiseE  = raise . toSomeError

catchE :: (RunExceptionM m SomeError, Error e) => m a -> (e -> m a) -> m a
catchE m k = do
  e <- try m
  case e of
    Right a -> return a
    Left se ->
      case fromSomeError se of
        Nothing -> raise se
        Just e' -> k e'

onError :: RunExceptionM m SomeError => m a -> m b -> m a
onError a b = do
  e <- try a
  case e of
    Right a -> return a
    Left se -> do
      _ <- b
      raise se

finallyE :: RunExceptionM m SomeError => m a -> m b -> m a
finallyE m e = do
  a <- m `onError` e
  _ <- e
  return a
