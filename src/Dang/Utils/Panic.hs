{-# LANGUAGE DeriveDataTypeable #-}

module Dang.Utils.Panic (
    panic
  ) where

import Dang.Utils.Pretty

import qualified Control.Exception as X
import           Data.Typeable ( Typeable )


-- | Non-recoverable exceptions.
data DangException = Panic (PPDoc ())
                     deriving (Typeable)

instance X.Exception DangException

instance Show DangException where
  show (Panic msg) = "panic: " ++ pretty msg

panic :: String -> PPDoc i -> a
panic m msg = X.throw (Panic (dropAnnots msg))
  where
  doc = vcat
    [ text ""
    , cutLine
    , hang (text "You have encountered a bug")
         2 (vcat [ text "Module: " <+> text m
                 , text "Message:" $$ msg ])
    , cutLine ]

  cutLine = text "--%<" <> text (replicate 76 '-')
