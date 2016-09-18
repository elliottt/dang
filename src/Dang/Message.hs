{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Dang.Message where

import Dang.Syntax.Location
import Dang.Utils.PP


data Error = ErrLexer
           | ErrParser
           | ErrDuplicateSig
           | ErrNoDeclForSig
           | ErrRnOverlap
           | ErrRnUnknown
           | ErrUnification
           | ErrInfiniteType
             deriving (Show,Eq,Ord)

describeError :: Error -> Doc
describeError ErrLexer        = text "Lexcial error"
describeError ErrParser       = text "Parse error"
describeError ErrDuplicateSig = text "Duplicate type signature for declaration"
describeError ErrNoDeclForSig = text "Type signature is missing a declaration"
describeError ErrRnOverlap    = text "Names overlap"
describeError ErrRnUnknown    = text "Name not in scope"
describeError ErrUnification  = text "Unification failed"
describeError ErrInfiniteType = text "Occurs check failed"

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
                       , msgSource :: !SrcRange
                       , msgDoc    :: Doc
                       } deriving (Show)

instance HasLoc Message where
  type LocSource Message = Source
  getLoc = msgSource


mkError :: Error -> SrcRange -> Doc -> Message
mkError err msgSource msgDoc = Message { msgType = Error err, .. }
{-# INLINE mkError #-}

isError :: Message -> Bool
isError Message { msgType = Error{} } = True
isError _                             = False

mkWarning :: Warning -> SrcRange -> Doc -> Message
mkWarning warn msgSource msgDoc = Message { msgType = Warning warn, .. }
{-# INLINE mkWarning #-}

isWarning :: Message -> Bool
isWarning Message { msgType = Warning{} } = True
isWarning _                               = False
