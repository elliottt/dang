{-# LANGUAGE OverloadedStrings #-}

module Dang.Utils.Ident (
    Namespace,
    packNamespaceLazy,

    Ident(),
    mkIdent,
    identText,
  ) where

import qualified Data.Text as S
import qualified Data.Text.Lazy as L


type Namespace = S.Text

packNamespaceLazy :: [L.Text] -> Namespace
packNamespaceLazy ns = L.toStrict (L.intercalate "." ns)


newtype Ident = Ident S.Text
                deriving (Show)

mkIdent :: S.Text -> Ident
mkIdent  = Ident

identText :: Ident -> S.Text
identText (Ident t) = t
