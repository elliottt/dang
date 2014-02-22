{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

module Dang.Monad (

    -- * Dang Monad
    Dang, DangM
  , runDang
  , runDangWithArgs

    -- ** Options
  , getOptions
  , whenDebugOpt

    -- ** IO
  , io

    -- ** Passes
  , pass
  , logInfo

    -- ** Locations
  , askLoc
  , withLoc

    -- ** Errors and Warnings
  , Error(..), addErr, addErrL, putErrs
  , Warning(..), addWarn, addWarnL, putWarns
  , collectMessages

    -- ** Error Recovery
  , try
  , tryMsgs
  ) where

import Dang.Options ( Options(..), DebugOpts(..), parseOptions )
import Dang.Utils.Location ( SrcLoc(NoLoc), ppLoc )
import Dang.Utils.Pretty

import Control.Applicative ( Applicative(..), (<$>) )
import Control.Monad ( MonadPlus(..), when )
import Control.Monad.Fix (MonadFix)
import Data.IORef
           ( IORef, newIORef, readIORef, writeIORef, modifyIORef'
           , atomicModifyIORef' )
import Data.Typeable (Typeable)
import MonadLib ( BaseM(..), RunM(..), ReaderT, ask )
import qualified Control.Exception as X


-- Monad -----------------------------------------------------------------------

data DangError = DangError deriving (Show,Typeable)

instance X.Exception DangError


data RO = RO { roOptions :: Options
             , roPPEnv   :: PPEnv
             , roLoc     :: IORef SrcLoc
             , roErrors  :: IORef [Error]
             , roWarns   :: IORef [Warning]
             , roLog     :: IORef Logger
             }


-- | Monads that can be used with base 'Dang' operations.
type DangM m = (Functor m, MonadPlus m, BaseM m Dang)

-- | The Dang monad.
newtype Dang a = Dang { getDang :: ReaderT RO IO a
                      } deriving (Functor,Applicative,MonadFix)

instance Monad Dang where
  {-# INLINE return #-}
  return x = Dang (return x)

  {-# INLINE (>>=) #-}
  m >>= f = Dang (getDang . f =<< getDang m)

  {-# INLINE fail #-}
  fail msg =
    do addErr (text msg)
       mzero

instance BaseM Dang Dang where
  {-# INLINE inBase #-}
  inBase = id

instance MonadPlus Dang where
  {-# INLINE mzero #-}
  mzero = Dang (inBase (X.throwIO DangError))

  {-# INLINE mplus #-}
  mplus l r =
    do ro <- askRO
       io (run ro l `X.catch` \ DangError -> run ro r)
    where
    run ro m = runM (getDang m) ro

instance RunM Dang a (Dang a) where
  {-# INLINE runM #-}
  runM = id

-- | Do some IO.
io :: BaseM m Dang => IO a -> m a
io m = inBase (Dang (inBase m))

-- | Get the operations, in a derived monad.
getOptions :: DangM m => m Options
getOptions  = roOptions `fmap` askRO

-- | Get the current location.
askLoc :: BaseM m Dang => m SrcLoc
askLoc  =
  do ro <- askRO
     io (readIORef (roLoc ro))

-- | Run a computation with a different location.
withLoc :: BaseM m Dang => SrcLoc -> m a -> m a
withLoc loc m =
  do ro   <- askRO
     loc' <- io (atomicModifyIORef' (roLoc ro) (\loc' -> (loc,loc')))
     a    <- m
     io (modifyIORef' (roLoc ro) (const loc'))
     return a

-- | Construct a new RO, to run a Dang computation.
newRO :: Options -> IO RO
newRO opts = RO opts defaultPPEnv <$> newIORef NoLoc
                                  <*> newIORef []
                                  <*> newIORef []
                                  <*> newIORef logSilent

askRO :: BaseM m Dang => m RO
askRO  = inBase (Dang ask)


-- | Turn a Dang operation into an IO operation.  This will allow exceptions to
-- escape.
runDang :: Options -> Dang a -> IO a
runDang opts m = do
  ro <- newRO opts
  runM (getDang m) ro

-- | Turn a Dang operation into an IO operation, using the provided arguments as
-- the command-line arguments.
runDangWithArgs :: [String] -> Dang a -> IO a
runDangWithArgs args m = do
  opts <- parseOptions args
  runDang opts m

ppDang :: BaseM dang Dang => PPDoc -> dang Doc
ppDang msg =
  do ro <- inBase (Dang ask)
     return (runPPM (roPPEnv ro) msg)

-- Options ---------------------------------------------------------------------

whenDebugOpt :: DangM m => (DebugOpts -> Bool) -> m () -> m ()
whenDebugOpt p m = do
  opts <- getOptions
  when (p (optDebugOpts opts)) m


-- Pass Logging ----------------------------------------------------------------

newtype Logger = Logger { logMessage :: Doc -> IO () }

chooseLogger :: Options -> String -> Logger
chooseLogger opts name
  | name `elem` optLogPasses opts = logAll
  | otherwise                     = logSilent

-- | Log no output.
logSilent :: Logger
logSilent  = Logger { logMessage  = \ _ -> return () }

-- | Log all output.
logAll :: Logger
logAll  = Logger { logMessage = print }

pass :: BaseM dang Dang => String -> dang a -> dang a
pass name body =
  do ro  <- inBase (Dang ask)
     old <- io $ do old <- readIORef (roLog ro)
                    writeIORef (roLog ro) (chooseLogger (roOptions ro) name)
                    return old
     logInfo (banner ("begin " ++ name))
     a   <- body
     logInfo (banner ("end " ++ name))
     io (writeIORef (roLog ro) old)
     return a

banner :: String -> PPDoc
banner msg = hcat [ text "--{", text msg, char '}'
                  , text (replicate (76 - length msg) '-') ]

logInfo :: BaseM dang Dang => PPDoc -> dang ()
logInfo msg =
  do doc <- ppDang msg
     ro  <- inBase (Dang ask)
     io $ do l <- readIORef (roLog ro)
             logMessage l doc



-- Messages --------------------------------------------------------------------

-- | Collect all the messages produced by a dang action.
collectMessages :: DangM m => m a -> m ([Error], [Warning], a)
collectMessages m =
  do res     <- replace ([],[])
     a       <- m
     (es,ws) <- replace res
     return (es,ws,a)
  where
  replace (es',ws') =
    do ro <- askRO
       io $ do es <- atomicModifyIORef' (roErrors ro) (\es -> (es',es))
               ws <- atomicModifyIORef' (roWarns  ro) (\ws -> (ws',ws))
               return (es,ws)


-- | Location-tagged error messages.
data Error = Error SrcLoc PPDoc

instance Pretty Error where
  ppr (Error loc msg) = hang (text "[error]" <+> ppLoc loc)
                           2 (msg $$ text "")

-- | Record an error with the current source location.
addErr :: (Pretty msg, BaseM m Dang) => msg -> m ()
addErr msg =
  do loc <- askLoc
     addErrL loc msg

-- | Add an error with a source location.
addErrL :: (Pretty msg, BaseM m Dang) => SrcLoc -> msg -> m ()
addErrL loc msg = putErrs [Error loc (ppr msg)]

-- | Primitive error recording.
putErrs :: BaseM m Dang => [Error] -> m ()
putErrs errs =
  do ro <- askRO
     io (modifyIORef' (roErrors ro) (\es -> es ++ errs))


-- | Location-tagged warning messages.
data Warning = Warning SrcLoc PPDoc

instance Pretty Warning where
  ppr (Warning loc msg) = hang (text "[warning]" <+> ppLoc loc)
                             2 (msg $$ text "")

-- | Add a warning with no location information.
addWarn :: (Pretty msg, BaseM m Dang) => msg -> m ()
addWarn msg =
  do loc <- askLoc
     addErrL loc msg

-- | Add a warning with location information
addWarnL :: (Pretty msg, BaseM m Dang) => SrcLoc -> msg -> m ()
addWarnL loc msg = putWarns [Warning loc (ppr msg)]

putWarns :: BaseM m Dang => [Warning] -> m ()
putWarns warns =
  do ro <- askRO
     io (modifyIORef' (roWarns ro) (\ws -> ws ++ warns))


-- Recovery --------------------------------------------------------------------

-- | Try to run a computation.  If it fails with 'fail' or 'mzero', return
-- Nothing.
try :: DangM m => m a -> m (Maybe a)
try m = fmap Just m `mplus` return Nothing

-- | A combination of 'try' and 'collectMessages'.
tryMsgs :: DangM m => m a -> m ([Error], [Warning], Maybe a)
tryMsgs  = collectMessages . try
