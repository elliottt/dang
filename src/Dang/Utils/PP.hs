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
    printDoc, hPrintDoc,

    -- ** Annotations
    Ann(..),
    annotate,

    -- ** Names
    getNameFormat,

    -- ** Class
    PP(..), pretty, pp,

    -- ** Combinators
    (<>), (<+>), ($$), ($+$),
    fsep, sep, hsep, cat, vcat, punctuate,
    optParens, parens, brackets, quotes,
    comma, commas,
    text, char, int, integer,
    hang, nest,
    emptyDoc,
  ) where

import Dang.Utils.Ident


import           Control.Monad (mplus)
import           Data.Int (Int64)
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (ReaderT,Id,runReaderT,runId,ask,local)
import qualified System.Console.ANSI as Ansi
import qualified System.Console.Terminal.Size as Term
import           System.IO (Handle,hPutStr,hPutChar,stdout)
import qualified Text.PrettyPrint.Annotated.HughesPJ as PJ


-- PP Config -------------------------------------------------------------------

data Config = Config { cfgNameDisp :: NameDisp
                     }

defaultConfig :: Config
defaultConfig  = Config { cfgNameDisp = mempty
                        }


-- Name Display ----------------------------------------------------------------

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


-- PP Environment --------------------------------------------------------------

data Env = Env { envConfig :: !Config
               , envPrec   :: !Int
               }

defaultEnv :: Config -> Env
defaultEnv envConfig = Env { envPrec = 0, .. }

getNameFormat :: Namespace -> Ident -> DocM (Maybe NameFormat)
getNameFormat ns i = DocM $
  do Env { .. } <- ask
     return $! formatName (cfgNameDisp envConfig) ns i


-- Monad -----------------------------------------------------------------------

newtype DocM a = DocM { unDocM :: ReaderT Env Id a
                      } deriving (Functor,Applicative,Monad)

type Doc = DocM (PJ.Doc Ann)

instance IsString (DocM (PJ.Doc Ann)) where
  fromString = text

instance Show (DocM (PJ.Doc Ann)) where
  show doc = show (runDoc defaultConfig doc)

runDoc :: Config -> Doc -> PJ.Doc Ann
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


-- Annotations -----------------------------------------------------------------

data Ann = AnnKeyword
         | AnnPunc
         | AnnLiteral
         | AnnComment
         | AnnError
           deriving (Show)

sgrFor :: Ann -> [Ansi.SGR]
sgrFor AnnKeyword = [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Green]
sgrFor AnnPunc    = [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Yellow]
sgrFor AnnLiteral = [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Magenta]
sgrFor AnnComment = [Ansi.SetColor Ansi.Foreground Ansi.Dull  Ansi.Green]
sgrFor AnnError   = [Ansi.SetColor Ansi.Background Ansi.Dull  Ansi.Red]

printDoc :: Config -> Doc -> IO ()
printDoc  = hPrintDoc stdout

-- | Print the document out, formatted for the console.
hPrintDoc :: Handle -> Config -> Doc -> IO ()
hPrintDoc h cfg doc =
  do mb  <- Term.hSize h
     len <- case mb of
              Just Term.Window { .. } -> return width
              Nothing                 -> return 80

     useAnsi <- Ansi.hSupportsANSI h

     fst $ PJ.fullRenderAnn PJ.PageMode len 1.5 (format useAnsi) (return (),[])
         $ runDoc cfg doc

  where

  format True PJ.AnnotStart (rest,stack) = (rest',drop 1 stack)
    where
    rest' = do Ansi.hSetSGR h []
               Ansi.hSetSGR h (concat stack)
               rest

  format True (PJ.AnnotEnd ann) (rest,stack) = (rest', sgrFor ann : stack)
    where
    rest' = do Ansi.hSetSGR h []
               rest

  format _ (PJ.NoAnnot td _) (rest,stack) = (fmt >> rest, stack)
    where
    fmt = case td of
            PJ.Chr  c -> hPutChar h c
            PJ.Str  s -> hPutStr  h s
            PJ.PStr s -> hPutStr  h s

  format False _ x = x

annotate :: Ann -> Doc -> Doc
annotate ann m = PJ.annotate ann <$> m


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

instance PP (DocM (PJ.Doc Ann)) where
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

liftDoc2 :: (PJ.Doc Ann -> PJ.Doc Ann -> PJ.Doc Ann) -> (Doc -> Doc -> Doc)
liftDoc2 f a b = f <$> a <*> b

(<>) :: Doc -> Doc -> Doc
(<>)  = liftDoc2 (PJ.<>)

(<+>) :: Doc -> Doc -> Doc
(<+>)  = liftDoc2 (PJ.<+>)

($$) :: Doc -> Doc -> Doc
($$)  = liftDoc2 (PJ.$$)

($+$) :: Doc -> Doc -> Doc
($+$)  = liftDoc2 (PJ.$+$)

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

quotes :: Doc -> Doc
quotes d = char '`' <> d <> char '`'

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

emptyDoc :: Doc
emptyDoc  = return PJ.empty
