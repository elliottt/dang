{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}

module Dang.Monad (
  Dang(), DangM, runDang,
  io,
  askLoc, addLoc, withLoc,
  try,

  -- ** Messages
  Messages,
  module Dang.Message,
  failErrors,
  collectMessages,
  addError,
  addWarning,
  putMessages,

  -- ** Re-exported
  mzero,
  mplus,
  ) where

import Dang.Message
import Dang.Syntax.Location (Source,SrcLoc,Located(..),SrcRange,HasLoc(..))
import Dang.Unique
import Dang.Utils.PP
import Dang.Utils.Panic

import           Control.Applicative (Alternative(..))
import qualified Control.Exception as X
import           Control.Monad (MonadPlus(..),guard)
import           Data.IORef
                     (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef'
                     ,modifyIORef')
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)
import           MonadLib (RunM(..), BaseM(..), ReaderT, ask)


type Messages = Seq.Seq Message

data RO = RO { roLoc    :: !(IORef SrcRange)
             , roMsgs   :: !(IORef Messages)
             , roSupply :: !(IORef Supply)
             }

newRO :: IO RO
newRO  =
  do roLoc    <- newIORef mempty
     roMsgs   <- newIORef Seq.empty
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
runDang' ro m =
  do res <- X.try (runM (unDang m) ro)
     case res of
       Right a -> return a
       Left p  ->
         do print (ppr (p :: Panic))
            X.throwIO DangError

io :: BaseM m Dang => IO a -> m a
io m = inBase (Dang (inBase m))

try :: DangM dang => dang a -> dang (Maybe a)
try m = (Just <$> m) `mplus` pure Nothing


-- Location Management ---------------------------------------------------------

-- | Retrieve the current source location.
askLoc :: DangM dang => dang SrcRange
askLoc  =
  do RO { .. } <- inBase (Dang ask)
     io (readIORef roLoc)

-- | Examine a located value, in the context of its location.
addLoc :: DangM dang => SrcLoc a -> (a -> dang b) -> dang b
addLoc Located { .. } f = withLoc locRange (f locValue)

-- | Run a sub-computation with a new source location.
withLoc :: (LocSource loc ~ Source, HasLoc loc, DangM dang) => loc -> dang a -> dang a
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
     let (es,ws) = Seq.partition isError ms
     guard (Seq.null es)
     putMessages ws
     return a

collectMessages :: DangM dang => dang a -> dang (a,Messages)
collectMessages m =
  do RO { .. } <- inBase (Dang ask)
     orig      <- io (atomicModifyIORef' roMsgs (\ orig -> (Seq.empty, orig)))
     a         <- m
     msgs      <- io (atomicModifyIORef' roMsgs (\ msgs -> (orig, msgs)))
     return (a,msgs)

putMessages :: DangM dang => Messages -> dang ()
putMessages ms = inBase $ Dang $
  do RO { .. } <- ask
     inBase (modifyIORef' roMsgs (ms Seq.><))

addMessage :: (PP msg, DangM dang) => MessageType -> msg -> dang ()
addMessage msgType msg = inBase $
  do msgSource <- askLoc
     putMessages (Seq.singleton Message { msgDoc = pp msg, .. })

addError :: (PP msg, DangM dang) => Error -> msg -> dang ()
addError e = addMessage (Error e)

addWarning :: (PP msg, DangM dang) => Warning -> msg -> dang ()
addWarning w = addMessage (Warning w)
