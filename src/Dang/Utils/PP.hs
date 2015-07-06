{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Dang.Utils.PP (
    Config(..), defaultConfig,
    Doc(),

    -- * Class
    PP(..), pretty, pp,

    -- * Combinators
    (<>), (<+>), ($$),
    fsep, sep, hsep, cat, vcat, punctuate,
    optParens, parens, brackets,
    comma, commas,
    text, char, int, integer,
    hang, nest
  ) where


import           Data.String (IsString(..))
import           MonadLib (ReaderT,Id,runReaderT,runId,ask,local)
import qualified Text.PrettyPrint.HughesPJ as PJ

import           Data.Int (Int64)


data Config = Config
              deriving (Show)

defaultConfig :: Config
defaultConfig  = Config

data Env = Env { envConfig :: !Config
               , envPrec   :: !Int
               } deriving (Show)

defaultEnv :: Config -> Env
defaultEnv envConfig = Env { envPrec = 0, .. }

newtype DocM a = DocM { unDocM :: ReaderT Env Id a
                      } deriving (Functor,Applicative,Monad)

type Doc = DocM PJ.Doc

instance IsString (DocM PJ.Doc) where
  fromString = text

instance Show (DocM PJ.Doc) where
  show doc = show (runDoc defaultConfig doc)

runDoc :: Config -> Doc -> PJ.Doc
runDoc cfg d = runId (runReaderT (defaultEnv cfg) (unDocM d))

getEnv :: DocM Env
getEnv  = DocM ask

withEnv :: Env -> DocM a -> DocM a
withEnv env m = DocM (local env (unDocM m))

getPrec :: DocM Int
getPrec  = envPrec <$> getEnv

withPrec :: Int -> DocM a -> DocM a
withPrec p m =
  do env <- getEnv
     withEnv env { envPrec = p } m


-- Class -----------------------------------------------------------------------

pretty :: PP a => a -> String
pretty a = PJ.render (runDoc defaultConfig (pp a))

pp :: PP a => a -> Doc
pp a = withPrec 0 (ppr a)

class PP a where
  ppr     :: a -> Doc
  pprList :: [a] -> Doc
  pprList as = brackets (fsep (commas (map pp as)))

instance PP a => PP [a] where
  ppr = pprList

instance PP (DocM PJ.Doc) where
  ppr = id
  {-# INLINE ppr #-}

instance PP Char where
  ppr     = char
  pprList = text

instance PP Integer where
  ppr = integer

instance PP Int where
  ppr = int

instance PP Int64 where
  ppr = ppr . toInteger


-- Combinators -----------------------------------------------------------------

liftDoc2 :: (PJ.Doc -> PJ.Doc -> PJ.Doc) -> (Doc -> Doc -> Doc)
liftDoc2 f a b = f <$> a <*> b

(<>) :: Doc -> Doc -> Doc
(<>)  = liftDoc2 (PJ.<>)

(<+>) :: Doc -> Doc -> Doc
(<+>)  = liftDoc2 (PJ.<+>)

($$) :: Doc -> Doc -> Doc
($$)  = liftDoc2 (PJ.$$)

fsep :: [Doc] -> Doc
fsep ds = PJ.fsep <$> sequence ds

sep :: [Doc] -> Doc
sep ds = PJ.sep <$> sequence ds

hsep :: [Doc] -> Doc
hsep ds = PJ.hsep <$> sequence ds

vcat :: [Doc] -> Doc
vcat ds = PJ.vcat <$> sequence ds

cat :: [Doc] -> Doc
cat ds = PJ.cat <$> sequence ds

optParens :: Int -> Doc -> Doc
optParens n doc =
  do p <- getPrec
     if p > n then parens doc
              else        doc

parens :: Doc -> Doc
parens  = fmap PJ.parens

brackets :: Doc -> Doc
brackets  = fmap PJ.brackets

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p xs = go xs
  where
  go [d]    = [d]
  go (d:ds) = d <> p : go ds
  go []     = []

comma :: Doc
comma  = char ','

commas :: [Doc] -> [Doc]
commas  = punctuate comma

text :: String -> Doc
text s = return (PJ.text s)

char :: Char -> Doc
char c = return (PJ.char c)

integer :: Integer -> Doc
integer i = return (PJ.integer i)

int :: Int -> Doc
int i = return (PJ.int i)

hang :: Doc -> Int -> Doc -> Doc
hang a i b = PJ.hang <$> a <*> pure i <*> b

nest :: Int -> Doc -> Doc
nest i d = PJ.nest i <$> d
