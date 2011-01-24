module Dang.FileName where

import System.FilePath


bcfile :: FilePath -> FilePath
bcfile path = dropExtension path <.> "bc"

ofile :: FilePath -> FilePath
ofile path = dropExtension path <.> "o"

ifile :: FilePath -> FilePath
ifile path = dropExtension path <.> "di"
