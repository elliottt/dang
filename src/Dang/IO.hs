{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module Dang.IO (
    loadFile
  , withROFile
  , withROBinaryFile
  , withWOBinaryFile
  , withClosedTempFile
  , withOpenTempFile
  , E.IOException

  , onFileNotFound

  , whenVerbosity
  , logInfo
  , logDebug
  ) where

import Dang.Monad

import MonadLib
import System.Directory
import System.IO
import System.IO.Error
import qualified Control.Exception as E
import qualified Data.ByteString   as S


-- | Read in a file as a strict ByteString.
loadFile :: BaseM m Dang => FilePath -> m S.ByteString
loadFile path = do
  logInfo ("load file: " ++ path)
  io (S.readFile path)

onFileNotFound :: RunExceptionM m SomeException
               => m a -> (E.IOException -> FilePath -> m a) -> m a
onFileNotFound m = catchJustE p m . uncurry
  where
  p e = do
    guard (isDoesNotExistError e)
    path <- ioeGetFileName e
    return (e,path)

dangTempDir :: FilePath
dangTempDir  = "/tmp/dang"

ensureTempDir :: BaseM m Dang => m ()
ensureTempDir  = io (createDirectoryIfMissing True dangTempDir)

ensureClosed :: BaseM m Dang => Handle -> m ()
ensureClosed h = io $ do
  b <- hIsClosed h
  unless b $ do
    hFlush h
    hClose h

withROFile :: BaseM m Dang => FilePath -> (Handle -> m a) -> m a
withROFile path k = do
  logInfo ("read file: " ++ path)
  h   <- io (openFile path ReadMode)
  res <- k h
  ensureClosed h
  return res

withROBinaryFile :: BaseM m Dang => FilePath -> (Handle -> m a) -> m a
withROBinaryFile path k = do
  logInfo ("read file[b]: " ++ path)
  h   <- io (openBinaryFile path ReadMode)
  res <- k h
  ensureClosed h
  return res

withWOBinaryFile :: BaseM m Dang => FilePath -> (Handle -> m a) -> m a
withWOBinaryFile path k = do
  logInfo ("write file[b]: " ++ path)
  h   <- io (openBinaryFile path WriteMode)
  res <- k h
  ensureClosed h
  return res

withOpenTempFile :: BaseM m Dang => (FilePath -> Handle -> m a) -> m a
withOpenTempFile k = do
  ensureTempDir
  (path,h) <- io (openTempFile "/tmp/dang" "dang.tmp")
  logInfo ("temp file: " ++ path)
  res      <- k path h
  ensureClosed h
  opts     <- getOptions
  unless (optKeepTempFiles opts) $ do
    logInfo ("removing: " ++ path)
    io (removeFile path)
  return res

withClosedTempFile :: BaseM m Dang => (FilePath -> m a) -> m a
withClosedTempFile k = withOpenTempFile $ \path h -> do
  io (hClose h)
  k path


-- Logging ---------------------------------------------------------------------

whenVerbosity :: BaseM m Dang => Verbosity -> m () -> m ()
whenVerbosity v m = do
  opts <- getOptions
  when (optVerbosity opts >= v) m

logString :: BaseM m Dang => String -> String -> m ()
logString label str =
  io $ putStrLn $ showChar '[' $ showString label $ showString "]\t" str

logInfo :: BaseM m Dang => String -> m ()
logInfo  = whenVerbosity 1 . logString "INFO"

logDebug :: BaseM m Dang => String -> m ()
logDebug  = whenVerbosity 2 . logString "DEBUG"
