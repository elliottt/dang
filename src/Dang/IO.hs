{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dang.IO (
    loadFile
  , withROFile
  , withClosedTempFile
  , withOpenTempFile
  , E.IOException

  , onFileNotFound
  ) where

import Dang.Monad

import MonadLib
import System.Directory
import System.IO
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

dangTempDir :: FilePath
dangTempDir  = "/tmp/dang"

ensureTempDir :: Dang ()
ensureTempDir  = inBase (createDirectoryIfMissing True dangTempDir)

withROFile :: FilePath -> (Handle -> Dang a) -> Dang a
withROFile path k = do
  h   <- inBase (openFile path ReadMode)
  res <- k h
  inBase (hClose h)
  return res

withOpenTempFile :: (FilePath -> Handle -> Dang a) -> Dang a
withOpenTempFile k = do
  ensureTempDir
  (path,h) <- inBase (openTempFile "/tmp/dang" "dang.tmp")
  res      <- k path h
  opts     <- ask
  inBase $ do
    b <- hIsClosed h
    unless b (hClose h)
    unless (optKeepTempFiles opts) (removeFile path)
    return res

withClosedTempFile :: (FilePath -> Dang a) -> Dang a
withClosedTempFile k = withOpenTempFile $ \path h -> do
  inBase (hClose h)
  k path
