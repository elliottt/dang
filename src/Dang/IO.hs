{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dang.IO (
    loadFile
  , E.IOException

  , onFileNotFound
  ) where

import Dang.Monad
import Error

import MonadLib
import System.IO.Error
import qualified Control.Exception as E
import qualified Data.ByteString   as S

instance Error E.IOException

-- | Lift IOExceptions into the DangError type.
handleIOE :: E.IOException -> IO (Either SomeError a)
handleIOE  = return . Left . toSomeError

-- | Read in a file as a strict ByteString.
loadFile :: FilePath -> Dang S.ByteString
loadFile path =
  liftEither =<< inBase (fmap Right (S.readFile path) `E.catch` handleIOE)

onFileNotFound :: Dang a -> (E.IOException -> FilePath -> Dang a) -> Dang a
onFileNotFound m = catchJustE p m . uncurry
  where
  p e = do
    guard (isDoesNotExistError e)
    path <- ioeGetFileName e
    return (e,path)
