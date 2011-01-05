{-# LANGUAGE EmptyDataDecls #-}

module Interface where

import Dang.IO
import Dang.Monad
import QualName
import ReadWrite
import qualified QualNameMap as QM

import Control.Monad (ap)
import Data.Int (Int32)
import Data.Serialize (runGet,runPut,Get,Putter,Serialize(get,put))
import System.FilePath (joinPath,(<.>),(</>))
import qualified Data.ByteString as S
import qualified Data.Map as Map


data FunDecl = FunDecl
  { funSymbol :: String
  , funArity  :: Int32
  } deriving Show

instance Serialize FunDecl where
  get    = FunDecl `fmap` get `ap` get
  put fd = put (funSymbol fd) >> put (funArity fd)

data Interface s = Interface
  { intFunDecls :: QM.QualNameMap FunDecl
  }

emptyInterface :: Interface RW
emptyInterface  = Interface
  { intFunDecls = QM.empty
  }

freezeInterface :: Interface i -> Interface R
freezeInterface i = Interface
  { intFunDecls = intFunDecls i
  }

-- | Merging creates an interface that can only be read.
mergeInterfaces :: Interface i -> Interface j -> Interface R
mergeInterfaces i1 i2 = Interface
  { intFunDecls = QM.union (intFunDecls i1) (intFunDecls i2)
  }

-- | Functions can be added to an interface in a read/write state.
addFunDecl :: QualName -> FunDecl -> Interface RW -> Interface RW
addFunDecl n s i = i
  { intFunDecls = QM.insert n s (intFunDecls i)
  }

-- | Functions can be looked up in an interface in any state.
findFunDecl :: QualName -> Interface i -> Maybe FunDecl
findFunDecl n = QM.lookup n . intFunDecls

-- | Locate an unqualified name in the interface.
findUnqualFunDecl :: Name -> Interface i -> [(QualName,FunDecl)]
findUnqualFunDecl n = QM.findUnqual n . intFunDecls

modContents :: QualName -> Interface i -> [(QualName,FunDecl)]
modContents qn = QM.findPrefix (qualPrefix qn ++ [qualSymbol qn]) . intFunDecls

ifaceFile :: QualName -> FilePath
ifaceFile qn = joinPath (qualPrefix qn) </> qualSymbol qn <.> "di"

openInterface :: QualName -> Dang (Interface RW)
openInterface qn =
  withROBinaryFile (ifaceFile qn) $ \ h -> do
    bytes <- io (S.hGetContents h)
    case runGet parseInterface bytes of
      Left err -> fail err
      Right a  -> return a

writeInterface :: QualName -> Interface RW -> Dang ()
writeInterface qn i =
  withWOBinaryFile (ifaceFile qn) $ \ h ->
    io (S.hPutStr h (runPut (renderInterface i)))

parseInterface :: Get (Interface RW)
parseInterface  = Interface `fmap` get

renderInterface :: Putter (Interface RW)
renderInterface (Interface i) = put i
