module Link where

import Dang.Monad
import Dang.Tool


rtsPath :: FilePath
rtsPath  = "rts/librts.a"

-- | Link together the given object files, writing the output to out.
link :: [FilePath] -> FilePath -> Dang ()
link ofiles out = sync gcc ("-o" : out : rtsPath : ofiles)
