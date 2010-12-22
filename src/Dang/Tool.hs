{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Tool where

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

logTool :: Tool -> [String] -> IO ()
logTool t extra = putStrLn $ unwords $
    [ " $", toolProg t ] ++ toolArgs t ++ extra

-- | Run a tool with optional extra arguments, waiting for it to exit.
sync :: Tool -> [String] -> Dang ()
sync t extra = inBase $ do
  logTool t extra
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
