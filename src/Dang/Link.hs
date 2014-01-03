{-# LANGUAGE Safe #-}

module Dang.Link where

import Dang.Monad
import Dang.Tool (sync,gcc)


rtsPath :: FilePath
rtsPath  = "rts/librts.a"

-- | Link together the given object files, writing the output to out.
link :: [FilePath] -> FilePath -> Dang ()
link ofiles out = pass "link" (sync gcc ("-o" : out : rtsPath : ofiles))
