{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

module Dang.Monad (
  Dang(), runDang,
  io,
  askLoc, addLoc, withLoc,
  try,

  -- ** Messages
  Message(..), MessageType(..), isError, isWarning,
  failErrors,
  collectMessages,
  formatMessage, formatMessages,
  addError,
  addWarning,

  -- ** Re-exported
  mzero,
  mplus,
  ) where

import Dang.Syntax.Location
           (Source(),Located(..),Range(..),HasLoc(..),rangeText,rangeUnderline
           ,Position(..))
import Dang.Unique
import Dang.Utils.PP

import           Control.Applicative (Alternative(..))
import qualified Control.Exception as X
import           Control.Monad (MonadPlus(..),guard)
import           Data.IORef
                     (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef'
                     ,modifyIORef')
import           Data.List (partition)
import qualified Data.Text.Lazy as L
import           Data.Typeable (Typeable)
import           MonadLib (RunM(..), BaseM(..), ReaderT, ask)


data RO = RO { roLoc    :: !(IORef Range)
             , roMsgs   :: !(IORef [Message])
             , roSupply :: !(IORef Supply)
             }

newRO :: IO RO
newRO  =
  do roLoc    <- newIORef mempty
     roMsgs   <- newIORef []
     roSupply <- newIORef initialSupply
     return RO { .. }

-- | Build an IO action that restores the previous state of the environment.
-- Messages aren't cleared out by the restore action, as it's useful to capture
-- the messages of a failed computation.
mkRestore :: RO -> IO (IO ())
mkRestore RO { .. } =
  do loc <- readIORef roLoc
     sup <- readIORef roSupply
     return $ do writeIORef roLoc    loc
                 writeIORef roSupply sup

newtype Dang a = Dang { unDang :: ReaderT RO IO a
                      } deriving (Functor,Applicative,Monad)

instance Alternative Dang where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Dang where
  mzero     = Dang (inBase (X.throwIO DangError))
  mplus a b = Dang $
    do ro      <- ask
       restore <- inBase (mkRestore ro)
       inBase (runDang' ro a `X.catch` \ DangError -> restore >> runDang' ro b)

instance BaseM Dang Dang where
  inBase = id

instance RunM Dang a (Dang a) where
  runM = id

instance SupplyM Dang where
  withSupply f =
    do RO { .. } <- Dang ask
       io (atomicModifyIORef' roSupply f)

-- | The identity to the 'Alternative' and 'MonadPlus' instances.
data DangError = DangError
                 deriving (Show,Typeable)

instance X.Exception DangError

type DangM m = (MonadPlus m, BaseM m Dang)

runDang :: Dang a -> IO a
runDang m =
  do ro <- newRO
     runDang' ro m

runDang' :: RO -> Dang a -> IO a
runDang' ro m = runM (unDang m) ro

io :: BaseM m Dang => IO a -> m a
io m = inBase (Dang (inBase m))

try :: DangM dang => dang a -> dang (Maybe a)
try m = (Just <$> m) `mplus` pure Nothing


-- Location Management ---------------------------------------------------------

-- | Retrieve the current source location.
askLoc :: DangM dang => dang Range
askLoc  =
  do RO { .. } <- inBase (Dang ask)
     io (readIORef roLoc)

-- | Examine a located value, in the context of its location.
addLoc :: DangM dang => Located a -> (a -> dang b) -> dang b
addLoc Located { .. } f = withLoc locRange (f locValue)

-- | Run a sub-computation with a new source location.
withLoc :: (HasLoc loc, DangM dang) => loc -> dang a -> dang a
withLoc loc body =
  do RO { .. } <- inBase (Dang ask)
     orig      <- io (atomicModifyIORef' roLoc (\ orig -> (getLoc loc, orig)))
     a         <- body
     io (modifyIORef' roLoc (const orig))
     return a


-- Errors and Warnings ---------------------------------------------------------

-- | Fail if errors are produced by the action given. Any warnings generated are
-- left in the environment when the action succeeds.
failErrors :: DangM dang => dang a -> dang a
failErrors m =
  do (a,ms) <- collectMessages m
     let (es,ws) = partition isError ms
     guard (null es)
     inBase (putMessages ws)
     return a


collectMessages :: DangM dang => dang a -> dang (a,[Message])
collectMessages m =
  do RO { .. } <- inBase (Dang ask)
     orig      <- io (atomicModifyIORef' roMsgs (\ orig -> ([], orig)))
     a         <- m
     msgs      <- io (atomicModifyIORef' roMsgs (\ msgs -> (orig, msgs)))
     return (a,msgs)


data Message = Message MessageType Range Doc
               deriving (Show)

instance PP Message where
  ppr (Message ty loc doc) = (pp ty <+> pp loc) $$ nest 2 doc

isError :: Message -> Bool
isError (Message Error _ _) = True
isError _                   = False

isWarning :: Message -> Bool
isWarning (Message Warning _ _) = True
isWarning _                     = False

data MessageType = Error
                 | Warning
                   deriving (Show)

instance PP MessageType where
  ppr Error   = text "[error]"
  ppr Warning = text "[warning]"

ppHeading :: String -> Doc
ppHeading msg =
  text "--" <+> text msg <+> text (replicate (80 - length msg - 4) '-')

formatMessages :: Source -> L.Text -> [Message] -> [Doc]
formatMessages src txt = map (formatMessage src txt)

formatMessage :: Source -> L.Text -> Message -> Doc
formatMessage src txt (Message ty loc doc) =
  ppHeading (show (pp ty <+> pp src <+> pp loc))
  $$ text ""
  $$ vcat source
  $$ nest (padding + 1) (rangeUnderline loc)
  $$ text ""
  $$ doc
  where
  cxtLines = 3

  context = L.lines (rangeText cxtLines loc txt)
  numbers = take (length context) [ posRow (rangeStart loc) - fromIntegral cxtLines .. ]
  padding = length (show (last numbers))
  source  = [ nest p (pp n) <> char '|' <> pp line
            | line <- context
            | n    <- numbers, let p = padding - length (show n) ]


putMessages :: [Message] -> Dang ()
putMessages ms = Dang $
  do RO { .. } <- ask
     inBase (modifyIORef' roMsgs (ms ++))

addMessage :: (PP msg, DangM dang) => MessageType -> msg -> dang ()
addMessage ty msg = inBase $
  do loc <- askLoc
     putMessages [Message ty loc (pp msg)]

addError :: (PP msg, DangM dang) => msg -> dang ()
addError  = addMessage Error

addWarning :: (PP msg, DangM dang) => msg -> dang ()
addWarning  = addMessage Warning
