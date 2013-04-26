module Dang.FileName where

import System.FilePath


type SourceFile = FilePath

-- | Turn a source file into the bitcode file it targets.
bcfile :: SourceFile -> FilePath
bcfile path = dropExtension path <.> "bc"

-- | Turn a source file name into the object file it targets.
ofile :: SourceFile -> FilePath
ofile path = dropExtension path <.> "o"

-- | Turn a source file into the interface file it targets.
ifile :: SourceFile -> FilePath
ifile path = dropExtension path <.> "di"
