{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dang.Utils.Pretty where

import           Control.Monad ( liftM2 )
import           Data.Int (Int8,Int16,Int32,Int64)
import           Data.List (intersperse)
import qualified Data.Set as Set
import           MonadLib ( runM, ReaderT, ask, local, Id )
import           System.IO (Handle)
import           Text.PrettyPrint.Annotated.HughesPJ (Span(..))
import qualified Text.PrettyPrint.Annotated.HughesPJ as PP

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

type PPDoc a = PPM (PP.Doc a)

runPPM :: PPEnv -> PPM a -> a
runPPM env m = runM (getPPM m) env

renderSpans :: PPEnv -> PPDoc i -> (String,[Span i])
renderSpans env m = PP.renderSpans (runPPM env m)

renderIO :: Handle
         -> PPEnv
         -> (Handle -> i -> IO ())
         -> (Handle -> i -> IO ())
         -> IO ()
         -> PPDoc i
         -> IO ()
renderIO h env start end done m =
  PP.renderDecoratedM (start h) (end h) putStr done (runPPM env m)

pretty :: PPDoc i -> String
pretty m = show (runPPM defaultPPEnv m)

dropAnnots :: PPDoc i -> PPDoc ()
dropAnnots m =
  do doc <- m
     return (const () `fmap` doc)

withPrec :: Int -> PPDoc i -> PPDoc i
withPrec p m = PPM $
  do env <- ask
     local env { ppePrec = p } (getPPM m)

ppPrec :: Pretty a i => Int -> a -> PPDoc i
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
quoted :: PPDoc i -> PPDoc i
quoted doc = char '`' <> doc <> char '`'

dot :: PPDoc i
dot  = char '.'

dots :: [PPDoc i] -> PPDoc i
dots  = hcat . intersperse dot

optParens :: Int -> PPDoc i -> PPDoc i
optParens p d =
  do env <- PPM ask
     if ppePrec env >= p
        then parens d
        else        d

layout :: [PPDoc i] -> PPDoc i
layout ds =
  do env <- PPM ask
     vcat $ if ppeLayout env
               then ds
               else list (char '{') (char ';') (char '}') ds

list :: PPDoc i -> PPDoc i -> PPDoc i -> [PPDoc i] -> [PPDoc i]
list open _ close []     = [open, close]
list open p close (d:ds) = open <+> d : foldr step [close] ds
  where
  step x rest = p <+> x : rest

pp :: Pretty a i => a -> PPDoc i
pp a = withPrec 0 (ppr a)

class Pretty a i where
  ppr :: a -> PPDoc i

instance Pretty (PP.Doc i) i where
  {-# INLINE ppr #-}
  ppr = return

instance Pretty (PPDoc i) i where
  {-# INLINE ppr #-}
  ppr = id

instance Pretty Char i where
  ppr = char

instance Pretty a i => Pretty [a] i where
  ppr as = fsep (list (char '[') comma (char ']') (map pp as))

instance Pretty a i => Pretty (Set.Set a) i where
  ppr = ppr . Set.toList

instance Pretty Int i where
  ppr i = int i

instance Pretty Int8 i where
  ppr i = integer (fromIntegral i)

instance Pretty Int16 i where
  ppr i = integer (fromIntegral i)

instance Pretty Int32 i where
  ppr i = integer (fromIntegral i)

instance Pretty Int64 i where
  ppr i = integer (fromIntegral i)

instance Pretty Integer i where
  ppr = integer

instance Pretty Float i where
  ppr = float

instance Pretty Double i where
  ppr = double

instance (Pretty a i, Pretty b i) => Pretty (a,b) i where
  ppr (a,b) = fsep (list (char '(') comma (char ')') [pp a, pp b])

instance (Pretty a i, Pretty b i, Pretty c i) => Pretty (a,b,c) i where
  ppr (a,b,c) = fsep (list (char '(') comma (char ')') [pp a, pp b, pp c])


-- Wrapper ---------------------------------------------------------------------

annotate :: i -> PPDoc i -> PPDoc i
annotate i = fmap (PP.annotate i)

empty :: PPDoc i
empty  = return PP.empty

text :: String -> PPDoc i
text str = return (PP.text str)

char :: Char -> PPDoc i
char c = return (PP.char c)

space :: PPDoc i
space  = return PP.space

int :: Int -> PPDoc i
int i = return (PP.int i)

integer :: Integer -> PPDoc i
integer i = return (PP.integer i)

float :: Float -> PPDoc i
float f = return (PP.float f)

double :: Double -> PPDoc i
double d = return (PP.double d)

punctuate :: PPDoc i -> [PPDoc i] -> [PPDoc i]
punctuate _ [] = []
punctuate p xs = go xs
  where
  go (x:rest) | null rest = [x]
              | otherwise = x <> p : go rest
  go []                   = [] -- not possible

parens :: PPDoc i -> PPDoc i
parens = fmap PP.parens

infixl 6 <+>
(<+>) :: PPDoc i -> PPDoc i -> PPDoc i
(<+>)  = liftM2 (PP.<+>)

infixl 6 <>
(<>) :: PPDoc i -> PPDoc i -> PPDoc i
(<>)  = liftM2 (PP.<>)

infixl 5 $$
($$) :: PPDoc i -> PPDoc i -> PPDoc i
($$)  = liftM2 (PP.$$)

comma :: PPDoc i
comma  = return PP.comma

pipe :: PPDoc i
pipe  = char '|'

commas :: [PPDoc i] -> [PPDoc i]
commas  = punctuate comma

pipes :: [PPDoc i] -> [PPDoc i]
pipes  = punctuate pipe

hsep :: [PPDoc i] -> PPDoc i
hsep ds = fmap PP.hsep (sequence ds)

sep :: [PPDoc i] -> PPDoc i
sep ds = fmap PP.sep (sequence ds)

fsep :: [PPDoc i] -> PPDoc i
fsep ds = fmap PP.fsep (sequence ds)

cat :: [PPDoc i] -> PPDoc i
cat ds = fmap PP.cat (sequence ds)

vcat :: [PPDoc i] -> PPDoc i
vcat ds = fmap PP.vcat (sequence ds)

hcat :: [PPDoc i] -> PPDoc i
hcat ds = fmap PP.hcat (sequence ds)

brackets :: PPDoc i -> PPDoc i
brackets  = fmap PP.brackets

braces :: PPDoc i -> PPDoc i
braces  = fmap PP.braces

nest :: Int -> PPDoc i -> PPDoc i
nest n = fmap (PP.nest n)

hang :: PPDoc i -> Int -> PPDoc i -> PPDoc i
hang p n q =
  do p' <- p
     q' <- q
     return (PP.hang p' n q')
