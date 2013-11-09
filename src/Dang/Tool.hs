{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Tool where

import Dang.IO
import Dang.Monad
import Dang.Utils.Pretty

import Control.Monad ( mzero )
import System.Exit ( ExitCode(..) )
import System.Process ( CreateProcess(..), proc, waitForProcess, createProcess )


data Tool = Tool
  { toolName :: String
  , toolProg :: String
  , toolArgs :: [String]
  } deriving (Show)

toolProcess :: Tool -> [String] -> CreateProcess
toolProcess t extra = proc (toolProg t) (toolArgs t ++ extra)

logTool :: DangM m => Tool -> [String] -> m ()
logTool t extra = logInfo $ unwords $ toolProg t : toolArgs t ++ extra

-- | Run a tool with optional extra arguments, waiting for it to exit.
sync :: DangM m => Tool -> [String] -> m ()
sync t extra = do
  logTool t extra
  exit <- io $ do
    (_,_,_,ph) <- createProcess (toolProcess t extra)
    waitForProcess ph
  case exit of
    ExitSuccess   -> return ()
    ExitFailure _ ->
      do addErr (text "Failed while running tool:" <+> text (toolName t))
         mzero

-- LLVM Tools ------------------------------------------------------------------

llvm_as :: Tool
llvm_as  = Tool
  { toolName = "LLVM Assembler"
  , toolProg = "llvm-as"
  , toolArgs = []
  }

llvm_ld :: Tool
llvm_ld  = Tool
  { toolName = "LLVM Linker"
  , toolProg = "llvm-ld"
  , toolArgs = []
  }

assembler :: Tool
assembler  = Tool
  { toolName = "Assembler"
  , toolProg = "as"
  , toolArgs = []
  }

gcc :: Tool
gcc  = Tool
  { toolName = "GCC"
  , toolProg = "gcc"
  , toolArgs = []
  }

llc :: Tool
llc  = Tool
  { toolName = "LLVM Code Generator"
  , toolProg = "llc"
  , toolArgs = []
  }
