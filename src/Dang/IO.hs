{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dang.IO (
    loadFile
  , E.IOException

  , onFileNotFound
  ) where

import Dang.Monad

import MonadLib
import System.IO.Error
import qualified Control.Exception as E
import qualified Data.ByteString   as S


-- | Read in a file as a strict ByteString.
loadFile :: FilePath -> Dang S.ByteString
loadFile path = inBase (S.readFile path)

onFileNotFound :: Dang a -> (E.IOException -> FilePath -> Dang a) -> Dang a
onFileNotFound m = catchJustE p m . uncurry
  where
  p e = do
    guard (isDoesNotExistError e)
    path <- ioeGetFileName e
    return (e,path)
