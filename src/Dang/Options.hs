module Dang.Options where

import Dang.Utils.Pretty

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
  , optLogPasses     :: [String]
  , optCompileOnly   :: Bool
  , optDebugOpts     :: DebugOpts
  } deriving Show

defaultOptions :: Options
defaultOptions  = Options
  { optKeepTempFiles = False
  , optAction        = NoAction
  , optLogPasses     = []
  , optCompileOnly   = False
  , optDebugOpts      = emptyDebugOpts
  }

data Action
  = Compile [FilePath]
  | Link [FilePath] [FilePath]
  | NoAction
    deriving Show

instance Pretty Action where
  ppr a = case a of
    Compile ms -> text "compile:" <+> sep (punctuate comma (map text ms))
    Link ss os -> text "link:" <+> sep (punctuate comma (map text (ss ++ os)))
    NoAction   -> text "no action"

actionSources :: Action -> [FilePath]
actionSources (Compile fs) = fs
actionSources (Link fs _)  = fs
actionSources NoAction     = []

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
  , Option "d" ["dump"] (ReqArg addLogPass "PASS{,PASS}")
    "Enable logging for PASS"
  , Option "c" [] (NoArg setCompileOnly)
    "Compile only"
  ] ++ debugOpts

handleHelp :: Option
handleHelp _ = do
  displayHelp []
  exitSuccess

setKeepTempFiles :: Option
setKeepTempFiles opts = return opts { optKeepTempFiles = True }

addLogPass :: String -> Option
addLogPass str opts = return opts { optLogPasses = optLogPasses opts ++ go str }
  where
  go xs = case break (== ',') xs of
            (pass,_:rest)             -> pass : go rest
            (pass,_     ) | null pass -> []
                          | otherwise -> [pass]

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
