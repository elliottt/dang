module Dang.Utils.Ident (
    Namespace,
    Ident(),
    mkIdent,
    identText,
  ) where

import qualified Data.Text as S

type Namespace = S.Text

newtype Ident = Ident S.Text
                deriving (Show)

mkIdent :: S.Text -> Ident
mkIdent  = Ident

identText :: Ident -> S.Text
identText (Ident t) = t
