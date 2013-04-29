module Dang.Pretty (
    module Dang.Pretty
  , module Text.PrettyPrint
  ) where

import Data.Int (Int8,Int16,Int32,Int64)
import Data.List (intersperse)
import Text.PrettyPrint
import qualified Data.Set as Set

pretty :: Pretty a => a -> String
pretty  = renderStyle fmt . ppr
  where
  fmt = Style PageMode 80 1.0

dot :: Doc
dot  = char '.'

commas :: [Doc] -> Doc
commas  = hcat . intersperse (comma <> space)

dots :: [Doc] -> Doc
dots  = hcat . intersperse dot

commaSep :: Doc -> Doc -> Doc
commaSep a b
  | isEmpty b = a
  | isEmpty a = b
  | otherwise = a <> comma <+> b

ppr :: Pretty a => a -> Doc
ppr  = pp 0

optParens :: Bool -> Doc -> Doc
optParens True = parens
optParens _    = id

optBraces :: Bool -> Doc -> Doc
optBraces True = braces
optBraces _    = id

semis :: [Doc] -> Doc
semis [] = empty
semis ds = foldr1 step ds
  where
  step d r = d $+$ semi <> space <> r

vcat' :: [Doc] -> Doc
vcat'  = foldr ($+$) empty

declBlock :: [Doc] -> Doc
declBlock []     = text "{}"
declBlock [d]    = char '{' <+> d <+> char '}'
declBlock (d:ds) = vcat' (char '{' <+> d : map (semi <+>) ds) $+$ char '}'

class Pretty a where
  pp     :: Int -> a -> Doc
  ppList :: Int -> [a] -> Doc
  ppList p as = hsep (map (pp p) as)

instance Pretty Bool where
  pp _ True  = int 1
  pp _ False = int 0

instance Pretty Char where
  pp _ = char
  ppList _ = text

instance Pretty a => Pretty (Maybe a) where
  pp p (Just a) = pp p a
  pp _ Nothing  = empty

instance Pretty a => Pretty [a] where
  pp p as = ppList p as

instance Pretty a => Pretty (Set.Set a) where
  pp p = ppList p . Set.toList

instance Pretty () where
  pp _ _ = empty

instance Pretty Int where
  pp _ i = int i

instance Pretty Int8 where
  pp _ i = integer (fromIntegral i)

instance Pretty Int16 where
  pp _ i = integer (fromIntegral i)

instance Pretty Int32 where
  pp _ i = integer (fromIntegral i)

instance Pretty Int64 where
  pp _ i = integer (fromIntegral i)

instance Pretty Integer where
  pp _ = integer

instance Pretty Float where
  pp _ = float

instance Pretty Double where
  pp _ = double

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pp _ (a,b) = parens (ppr a <> comma <+> ppr b)

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pp _ (a,b,c) = parens (commas [ppr a, ppr b, ppr c])
