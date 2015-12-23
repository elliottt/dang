{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Dang.Utils.PP (
    -- * Config
    Config(..), defaultConfig,

    -- ** Name display
    NameDisp(),
    neverQualify,
    alwaysQualify,
    NameFormat(..),
    formatName,

    -- * Pretty-printer
    Doc(),

    -- ** Names
    getNameFormat,

    -- ** Class
    PP(..), pretty, pp,

    -- ** Combinators
    (<>), (<+>), ($$),
    fsep, sep, hsep, cat, vcat, punctuate,
    optParens, parens, brackets,
    comma, commas,
    text, char, int, integer,
    hang, nest
  ) where

import Dang.Utils.Ident


import           Control.Monad (mplus)
import           Data.Int (Int64)
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (ReaderT,Id,runReaderT,runId,ask,local)
import qualified Text.PrettyPrint.HughesPJ as PJ



data Config = Config { cfgNameDisp :: NameDisp
                     }

defaultConfig :: Config
defaultConfig  = Config { cfgNameDisp = mempty
                        }


-- | How to display names, inspired by the GHC `Outputable` module.
data NameDisp = EmptyNameDisp
              | NameDisp (Namespace -> Ident -> Maybe NameFormat)

instance Monoid NameDisp where
  mempty                              = EmptyNameDisp

  mappend (NameDisp f)  (NameDisp g)  = NameDisp (\ns i -> f ns i `mplus` g ns i)
  mappend EmptyNameDisp b             = b
  mappend a             EmptyNameDisp = a

neverQualify :: Namespace -> NameDisp
neverQualify ns = NameDisp $ \ ns' _ ->
  if ns == ns'
     then return UnQualified
     else Nothing

alwaysQualify :: Namespace -> NameDisp
alwaysQualify ns = NameDisp $ \ ns' _ ->
  if ns == ns'
     then return (Qualified ns)
     else Nothing


data NameFormat = UnQualified
                | Qualified !Namespace
                  deriving (Show)

-- | Lookup formatting information for a name. A result of 'Nothing' indicates
-- that the name is not in scope.
formatName :: NameDisp -> Namespace -> Ident -> Maybe NameFormat
formatName EmptyNameDisp = \ _ _ -> Nothing
formatName (NameDisp f)  = f




data Env = Env { envConfig :: !Config
               , envPrec   :: !Int
               }

defaultEnv :: Config -> Env
defaultEnv envConfig = Env { envPrec = 0, .. }

getNameFormat :: Namespace -> Ident -> DocM (Maybe NameFormat)
getNameFormat ns i = DocM $
  do Env { .. } <- ask
     return $! formatName (cfgNameDisp envConfig) ns i

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

instance PP T.Text where
  ppr s = text (T.unpack s)

instance PP L.Text where
  ppr s = text (L.unpack s)

instance PP Ident where
  ppr ident = ppr (identText ident)


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
