{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Monad where

import Error

import Control.Applicative (Applicative)
import Data.Typeable (Typeable)
import MonadLib
import qualified Control.Exception as E
import qualified Data.ByteString   as S

data DangError
  = DangError String
  | IOError String
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

-- | Lift IOExceptions into the DangError type.
handleIOE :: E.IOException -> IO (Either SomeError a)
handleIOE e = return (Left (toSomeError (IOError (show e))))

-- | Read in a file as a strict ByteString.
loadFile :: FilePath -> Dang S.ByteString
loadFile path =
  liftEither =<< inBase (fmap Right (S.readFile path) `E.catch` handleIOE)

raiseDang :: String -> Dang a
raiseDang  = raiseE . DangError
