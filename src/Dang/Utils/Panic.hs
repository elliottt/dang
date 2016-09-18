{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Dang.Utils.Panic (
    panic,
    Panic(),
    HasCallStack,
  ) where

import Dang.Utils.PP

import qualified Control.Exception as X
import           Data.Typeable (Typeable)
import           GHC.Stack


data Panic = Panic CallStack Doc
             deriving (Show,Typeable)

instance X.Exception Panic

instance PP Panic where
  ppr (Panic cxt msg) =
    vcat [ line "PANIC"
         , hang (vcat (map ppCxt stack) <> char ':') 2 msg
         , line "PANIC"
         ]
    where
    -- remove the use of `panic`
    stack = drop 1 (getCallStack cxt)

    line str =
      let len = 80 - length str - 4
       in text "--" <+> text str <+> text (replicate len '-')

    ppCxt (fun,SrcLoc { .. }) = text srcLocModule
                             <> char ':'
                             <> text fun
                             <> parens(ppr srcLocStartLine <> char ','
                                       <> ppr srcLocStartCol)


panic :: (HasCallStack, PP msg) => msg -> a
panic msg =
  let stack = freezeCallStack callStack
   in X.throw (Panic stack (pp msg))
