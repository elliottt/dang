{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dang.Utils.Pretty (
    module Dang.Utils.Pretty
  , C.Color()
  , C.black, C.red, C.green, C.yellow, C.blue, C.magenta, C.cyan, C.white
  , C.Mode()
  , C.fg, C.bg, C.reset, C.bold, C.underscore, C.blink
  , C.Command()
  , C.setGraphics
  , PP.Doc
  ) where

import qualified Dang.Colors as C

import           Control.Monad ( liftM2 )
import           Data.Int (Int8,Int16,Int32,Int64)
import           Data.List (intersperse)
import qualified Data.Set as Set
import           MonadLib ( runM, ReaderT, ask, local, Id )
import qualified Text.PrettyPrint as PP

-- Pretty-printing Monad -------------------------------------------------------

data PPEnv = PPEnv { ppePrec :: Int
                     -- ^ The current precedence level.
                   , ppeLayout :: Bool
                     -- ^ If layout should be used when printing.
                   , ppePrintLevels :: Bool
                     -- ^ Whether or not names should be annotated with their
                     -- level.
                   , ppePrintQual :: Bool
                     -- ^ Whether or not names should be printed
                     -- fully-qualified, or as they were parsed in the source.
                   , ppeColor :: Bool
                     -- ^ Whether or not to show colors
                   } deriving (Show)

defaultPPEnv :: PPEnv
defaultPPEnv  = PPEnv { ppePrec        = 0
                      , ppeColor       = True
                      , ppeLayout      = True
                      , ppePrintLevels = False
                      , ppePrintQual   = False }

newtype PPM a = PPM { getPPM :: ReaderT PPEnv Id a
                    } deriving (Functor,Applicative,Monad)


type PPDoc = PPM PP.Doc

runPPM :: PPEnv -> PPM a -> a
runPPM env m = runM (getPPM m) env

pretty :: Pretty a => a -> String
pretty a = PP.renderStyle fmt (runPPM defaultPPEnv (pp a))
  where
  fmt = PP.Style PP.PageMode 80 1.0

ppPrec :: Pretty a => Int -> a -> PPDoc
ppPrec p a = PPM $
  do env <- ask
     local env { ppePrec = p } (getPPM (ppr a))

getPrec :: PPM Int
getPrec  = PPM (ppePrec `fmap` ask)

getPrintLevels :: PPM Bool
getPrintLevels  = PPM (ppePrintLevels `fmap` ask)

getPrintQual :: PPM Bool
getPrintQual  = PPM (ppePrintQual `fmap` ask)

-- Utility ---------------------------------------------------------------------

-- | Quote with backticks.
quoted :: PPDoc -> PPDoc
quoted doc = char '`' <> doc <> char '`'

withGraphics :: [C.Mode] -> PPDoc -> PPDoc
withGraphics mode body =
  do env <- PPM ask
     if ppeColor env
        then return (PP.zeroWidthText (C.escape (C.setGraphics mode)))
          <> body
          <> return (PP.zeroWidthText (C.escape (C.setGraphics [C.reset])))
        else body

dot :: PPDoc
dot  = char '.'

dots :: [PPDoc] -> PPDoc
dots  = hcat . intersperse dot

optParens :: Int -> PPDoc -> PPDoc
optParens p d =
  do env <- PPM ask
     if ppePrec env >= p
        then parens d
        else        d

layout :: [PPDoc] -> PPDoc
layout ds =
  do env <- PPM ask
     vcat $ if ppeLayout env
               then ds
               else list (char '{') (char ';') (char '}') ds

list :: PPDoc -> PPDoc -> PPDoc -> [PPDoc] -> [PPDoc]
list open _ close []     = [open, close]
list open p close (d:ds) = open <+> d : foldr step [close] ds
  where
  step x rest = p <+> x : rest

pp :: Pretty a => a -> PPDoc
pp a = ppPrec 0 (ppr a)

class Pretty a where
  ppr :: a -> PPDoc

instance Pretty PP.Doc where
  {-# INLINE ppr #-}
  ppr = return

instance Pretty PPDoc where
  {-# INLINE ppr #-}
  ppr = id

instance Pretty Char where
  ppr = char

instance Pretty a => Pretty [a] where
  ppr as = fsep (list (char '[') comma (char ']') (map pp as))

instance Pretty a => Pretty (Set.Set a) where
  ppr = ppr . Set.toList

instance Pretty Int where
  ppr i = int i

instance Pretty Int8 where
  ppr i = integer (fromIntegral i)

instance Pretty Int16 where
  ppr i = integer (fromIntegral i)

instance Pretty Int32 where
  ppr i = integer (fromIntegral i)

instance Pretty Int64 where
  ppr i = integer (fromIntegral i)

instance Pretty Integer where
  ppr = integer

instance Pretty Float where
  ppr = float

instance Pretty Double where
  ppr = double

instance (Pretty a, Pretty b) => Pretty (a,b) where
  ppr (a,b) = fsep (list (char '(') comma (char ')') [pp a, pp b])

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  ppr (a,b,c) = fsep (list (char '(') comma (char ')') [pp a, pp b, pp c])


-- Wrapper ---------------------------------------------------------------------

empty :: PPDoc
empty  = return PP.empty

text :: String -> PPDoc
text str = return (PP.text str)

char :: Char -> PPDoc
char c = return (PP.char c)

space :: PPDoc
space  = return PP.space

int :: Int -> PPDoc
int i = return (PP.int i)

integer :: Integer -> PPDoc
integer i = return (PP.integer i)

float :: Float -> PPDoc
float f = return (PP.float f)

double :: Double -> PPDoc
double d = return (PP.double d)

punctuate :: PPDoc -> [PPDoc] -> [PPDoc]
punctuate _ [] = []
punctuate p xs = go xs
  where
  go (x:rest) | null rest = [x]
              | otherwise = x <> p : go rest
  go []                   = [] -- not possible

parens :: PPDoc -> PPDoc
parens = fmap PP.parens

infixl 6 <+>
(<+>) :: PPDoc -> PPDoc -> PPDoc
(<+>)  = liftM2 (PP.<+>)

infixl 6 <>
(<>) :: PPDoc -> PPDoc -> PPDoc
(<>)  = liftM2 (PP.<>)

infixl 5 $$
($$) :: PPDoc -> PPDoc -> PPDoc
($$)  = liftM2 (PP.$$)

comma :: PPDoc
comma  = return PP.comma

pipe :: PPDoc
pipe  = char '|'

commas :: [PPDoc] -> [PPDoc]
commas  = punctuate comma

pipes :: [PPDoc] -> [PPDoc]
pipes  = punctuate pipe

hsep :: [PPDoc] -> PPDoc
hsep ds = fmap PP.hsep (sequence ds)

sep :: [PPDoc] -> PPDoc
sep ds = fmap PP.sep (sequence ds)

fsep :: [PPDoc] -> PPDoc
fsep ds = fmap PP.fsep (sequence ds)

cat :: [PPDoc] -> PPDoc
cat ds = fmap PP.cat (sequence ds)

vcat :: [PPDoc] -> PPDoc
vcat ds = fmap PP.vcat (sequence ds)

hcat :: [PPDoc] -> PPDoc
hcat ds = fmap PP.hcat (sequence ds)

brackets :: PPDoc -> PPDoc
brackets  = fmap PP.brackets

braces :: PPDoc -> PPDoc
braces  = fmap PP.braces

nest :: Int -> PPDoc -> PPDoc
nest n = fmap (PP.nest n)

hang :: PPDoc -> Int -> PPDoc -> PPDoc
hang p n q =
  do p' <- p
     q' <- q
     return (PP.hang p' n q')
