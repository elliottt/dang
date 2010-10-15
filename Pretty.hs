module Pretty (
    module Pretty
  , module Text.PrettyPrint
  ) where

import Data.Int (Int32)
import Data.List (intersperse)
import Text.PrettyPrint

pretty :: Pretty a => a -> String
pretty  = render . pp 0

dot :: Doc
dot  = char '.'

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

ppr :: Pretty a => a -> Doc
ppr  = pp 0

class Pretty a where
  pp     :: Int -> a -> Doc
  ppList :: Int -> [a] -> Doc
  ppList p as = hsep (map (pp p) as)

instance Pretty a => Pretty (Maybe a) where
  pp p (Just a) = pp p a
  pp _ Nothing  = empty

instance Pretty a => Pretty [a] where
  pp p as = ppList p as

instance Pretty Int32 where
  pp _ i = integer (fromIntegral i)
