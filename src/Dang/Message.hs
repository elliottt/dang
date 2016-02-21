{-# LANGUAGE RecordWildCards #-}

module Dang.Message where

import Dang.Syntax.Location
import Dang.Utils.PP


data Error = ErrLexer
           | ErrParser
           | ErrRnOverlap
           | ErrRnUnknown
             deriving (Show,Eq,Ord)

describeError :: Error -> Doc
describeError ErrLexer     = text "Lexcial error"
describeError ErrParser    = text "Parse error"
describeError ErrRnOverlap = text "Names overlap"
describeError ErrRnUnknown = text "Name not in scope"

data Warning = WarnRnShadowing
               deriving (Show,Eq,Ord)

describeWarning :: Warning -> Doc
describeWarning WarnRnShadowing = text "Name shadowing"

data MessageType = Error Error
                 | Warning Warning
                   deriving (Show,Eq,Ord)

describeMessageType :: MessageType -> Doc
describeMessageType (Error err)    = describeError err
describeMessageType (Warning warn) = describeWarning warn

data Message = Message { msgType   :: !MessageType
                       , msgSource :: !Range
                       , msgDoc    :: Doc
                       } deriving (Show)

instance Eq Message where
  a == b = msgSource a == msgSource b && msgType a == msgType b
  a /= b = msgSource a /= msgSource b && msgType a /= msgType b
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance Ord Message where
  compare a b =
    case compare (msgSource a) (msgSource b) of
      EQ  -> compare (msgType a) (msgType b)
      cmp -> cmp
  {-# INLINE compare #-}

instance HasLoc Message where
  getLoc = msgSource


mkError :: Error -> Range -> Doc -> Message
mkError err msgSource msgDoc = Message { msgType = Error err, .. }
{-# INLINE mkError #-}

isError :: Message -> Bool
isError Message { msgType = Error{} } = True
isError _                             = False

mkWarning :: Warning -> Range -> Doc -> Message
mkWarning warn msgSource msgDoc = Message { msgType = Warning warn, .. }
{-# INLINE mkWarning #-}

isWarning :: Message -> Bool
isWarning Message { msgType = Warning{} } = True
isWarning _                               = False
