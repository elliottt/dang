{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Dang.Tool where

import Dang.IO
import Dang.Monad

import Data.Typeable (Typeable)
import MonadLib
import System.Exit
import System.Process

data ToolError = ToolError Tool
    deriving (Show,Typeable)

instance Exception ToolError


data Tool = Tool
  { toolName :: String
  , toolProg :: String
  , toolArgs :: [String]
  } deriving (Show,Typeable)

toolProcess :: Tool -> [String] -> CreateProcess
toolProcess t extra = proc (toolProg t) (toolArgs t ++ extra)

logTool :: BaseM m Dang => Tool -> [String] -> m ()
logTool t extra = logInfo $ unwords $ toolProg t : toolArgs t ++ extra

-- | Run a tool with optional extra arguments, waiting for it to exit.
sync :: BaseM m Dang => Tool -> [String] -> m ()
sync t extra = do
  logTool t extra
  io $ do
    (_,_,_,ph) <- createProcess (toolProcess t extra)
    exit       <- waitForProcess ph
    case exit of
      ExitSuccess   -> return ()
      ExitFailure _ -> raiseE (ToolError t)

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
