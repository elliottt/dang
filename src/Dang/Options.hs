module Dang.Options where

import Control.Monad ( foldM )
import Data.List ( partition )
import System.Console.GetOpt
    ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import System.Environment ( getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath ( takeExtension )


-- Environment Configuration ---------------------------------------------------

data Options = Options
  { optKeepTempFiles :: Bool
  , optAction        :: Action
  , optVerbosity     :: Verbosity
  , optCompileOnly   :: Bool
  , optDebugOpts     :: DebugOpts
  } deriving Show

defaultOptions :: Options
defaultOptions  = Options
  { optKeepTempFiles = False
  , optAction        = NoAction
  , optVerbosity     = 0
  , optCompileOnly   = False
  , optDebugOpts      = emptyDebugOpts
  }

data Action
  = Compile [FilePath]
  | Link [FilePath] [FilePath]
  | NoAction
    deriving Show

actionSources :: Action -> [FilePath]
actionSources (Compile fs) = fs
actionSources (Link fs _)  = fs
actionSources NoAction     = []

type Verbosity = Int

type Option = Options -> IO Options

parseOptions :: [String] -> IO Options
parseOptions args =
  case getOpt Permute options args of
    (os,fs,[]) -> buildOptions os fs
    (_,_,errs) -> do
      displayHelp errs
      exitFailure

buildOptions :: [Option] -> [String] -> IO Options
buildOptions os fs = do
  opts <- foldM (flip id) defaultOptions os
  return opts { optAction = inferAction opts fs }

inferAction :: Options -> [String] -> Action
inferAction opts fs
  | optCompileOnly opts = Compile sources
  | otherwise           = Link sources objs
  where
  (sources,objs) = partition (\f -> takeExtension f == ".dg") fs

displayHelp :: [String] -> IO ()
displayHelp errs = do
  prog <- getProgName
  let banner = unlines (errs ++ ["Usage: " ++ prog ++ " [options] source"])
  putStr (usageInfo banner options)

options :: [OptDescr Option]
options  =
  [ Option "h" ["help"] (NoArg handleHelp)
    "Display this message"
  , Option [] ["keep-temp-files"] (NoArg setKeepTempFiles)
    "Don't remove temp files created during compilation"
  , Option "v" ["verbosity"] (ReqArg setVerbosity "LEVEL")
    "Set the logging verbosity"
  , Option "c" [] (NoArg setCompileOnly)
    "Compile only"
  ] ++ debugOpts

handleHelp :: Option
handleHelp _ = do
  displayHelp []
  exitSuccess

setKeepTempFiles :: Option
setKeepTempFiles opts = return opts { optKeepTempFiles = True }

setVerbosity :: String -> Option
setVerbosity msg opts =
  case reads msg of
    [(v,[])] -> return opts { optVerbosity = v }
    _        -> fail ("Unable to parse verbosity from: " ++ msg)

setCompileOnly :: Option
setCompileOnly opts = return opts { optCompileOnly = True }

updateDebugOpts :: (DebugOpts -> IO DebugOpts) -> Option
updateDebugOpts k opts = do
  d <- k (optDebugOpts opts)
  return opts { optDebugOpts = d }

data DebugOpts = DebugOpts
  { dbgDumpLLVM :: Bool
  } deriving Show

emptyDebugOpts :: DebugOpts
emptyDebugOpts  = DebugOpts
  { dbgDumpLLVM = False
  }

debugOpts :: [OptDescr Option]
debugOpts  =
  [ Option "" ["dump-llvm"] (NoArg (updateDebugOpts setDumpLLVM))
    "Dump the generated LLVM assembly"
  ]

setDumpLLVM :: DebugOpts -> IO DebugOpts
setDumpLLVM opts = return opts { dbgDumpLLVM = True }
