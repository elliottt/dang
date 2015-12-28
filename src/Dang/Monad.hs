{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dang.Monad (
  Dang(), runDang,
  io,
  askLoc, addLoc, withLoc,

  -- ** Messages
  Message(..), MessageType(..),
  collectMessages,
  addError,
  addWarning
  ) where

import Dang.Syntax.Location (Located(..),Range,HasLoc(..))
import Dang.Unique
import Dang.Utils.PP

import           Control.Applicative (Alternative(..))
import qualified Control.Exception as X
import           Control.Monad (MonadPlus(..))
import           Data.IORef
                     (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef'
                     ,modifyIORef')
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
mkRestore :: RO -> IO (IO ())
mkRestore RO { .. } =
  do loc  <- readIORef roLoc
     msgs <- readIORef roMsgs
     return $ do writeIORef roLoc  loc
                 writeIORef roMsgs msgs

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

data MessageType = Error
                 | Warning
                   deriving (Show)

instance PP MessageType where
  ppr Error   = text "[error]"
  ppr Warning = text "[warning]"

addMessage :: (PP msg, DangM dang) => MessageType -> msg -> dang ()
addMessage ty msg =
  do RO { .. } <- inBase (Dang ask)
     io $ do loc <- readIORef roLoc
             modifyIORef' roMsgs (Message ty loc (pp msg):)

addError :: (PP msg, DangM dang) => msg -> dang ()
addError  = addMessage Error

addWarning :: (PP msg, DangM dang) => msg -> dang ()
addWarning  = addMessage Warning
