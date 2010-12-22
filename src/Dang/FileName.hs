module Dang.FileName where

import System.FilePath


ofile :: FilePath -> FilePath
ofile path = dropExtension path <.> "o"

ifile :: FilePath -> FilePath
ifile path = dropExtension path <.> "di"
