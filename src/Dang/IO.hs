{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

module Dang.IO (
    loadFile
  , withROFile
  , withROBinaryFile
  , withWOBinaryFile
  , withClosedTempFile
  , withOpenTempFile
  , E.IOException
  ) where

import Dang.Monad
import Dang.Options
import Dang.Utils.Pretty

import Control.Monad ( unless, mzero )
import System.Directory ( createDirectoryIfMissing, removeFile )
import System.IO
import qualified Control.Exception as E
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L


-- | Read in a file as a strict ByteString.
loadFile :: DangM m => FilePath -> m L.Text
loadFile path = do
  logInfo (text "load file: " <+> text path)
  e <- io (E.try (L.readFile path))
  handle e
  where
  handle :: DangM m => Either E.IOException L.Text -> m L.Text
  handle e = case e of
    Right bytes -> return bytes
    Left _x     ->
      do addErr (text "Unable to open file" <+> quoted (text path))
         mzero

dangTempDir :: FilePath
dangTempDir  = "/tmp/dang"

ensureTempDir :: DangM m => m ()
ensureTempDir  = io (createDirectoryIfMissing True dangTempDir)

ensureClosed :: DangM m => Handle -> m ()
ensureClosed h = io $ do
  b <- hIsClosed h
  unless b $ do
    hFlush h
    hClose h

withROFile :: DangM m => FilePath -> (Handle -> m a) -> m a
withROFile path k = do
  logInfo (text "read file:" <+> text path)
  h   <- io (openFile path ReadMode)
  res <- k h
  ensureClosed h
  return res

withROBinaryFile :: DangM m => FilePath -> (Handle -> m a) -> m a
withROBinaryFile path k = do
  logInfo (text "read file[b]:" <+> text path)
  h   <- io (openBinaryFile path ReadMode)
  res <- k h
  ensureClosed h
  return res

withWOBinaryFile :: DangM m => FilePath -> (Handle -> m a) -> m a
withWOBinaryFile path k = do
  logInfo (text "write file[b]:" <+> text path)
  h   <- io (openBinaryFile path WriteMode)
  res <- k h
  ensureClosed h
  return res

withOpenTempFile :: DangM m => (FilePath -> Handle -> m a) -> m a
withOpenTempFile k = do
  ensureTempDir
  (path,h) <- io (openTempFile "/tmp/dang" "dang.tmp")
  logInfo (text "temp file:" <+> text path)
  res      <- k path h
  ensureClosed h
  opts     <- getOptions
  unless (optKeepTempFiles opts) $ do
    logInfo (text "removing:" <+> text path)
    io (removeFile path)
  return res

withClosedTempFile :: DangM m => (FilePath -> m a) -> m a
withClosedTempFile k = withOpenTempFile $ \path h -> do
  io (hClose h)
  k path
