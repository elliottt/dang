{-# LANGUAGE Safe #-}

module Dang.Link where

import Dang.IO (logStage)
import Dang.Monad (Dang)
import Dang.Tool (sync,gcc)


rtsPath :: FilePath
rtsPath  = "rts/librts.a"

-- | Link together the given object files, writing the output to out.
link :: [FilePath] -> FilePath -> Dang ()
link ofiles out = do
  logStage "link"
  sync gcc ("-o" : out : rtsPath : ofiles)
