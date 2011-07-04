module Interface2 (
    Interface()

    -- * Versioning
  , Version(..)
  , currentVersion

    -- * Symbols
  , FunSymbol(..)

    -- * Creation
  , moduleInterface

    -- * Query
  , lookupFunSymbol
  , lookupKind

    -- * Serialization
  , getInterface
  , putInterface
  ) where

import Dang.IO
import Dang.Monad
import QualName
import Syntax.AST
import TypeChecker.Types

import Control.Applicative ((<$>),(<*>))
import Data.Maybe (fromJust)
import Data.Serialize
    (Get,Putter,get,put,getWord8,putWord8,getMapOf,putMapOf,runPut,runGet)
import Data.Word (Word8)
import System.FilePath (joinPath,(<.>),(</>))
import qualified Data.ByteString as S
import qualified Data.Map as Map

type IFaceTypes = Map.Map QualName FunSymbol
type IFaceKinds = Map.Map QualName Kind

data Interface = Interface
  { ifaceVersion   :: Version
  , ifaceNamespace :: Namespace
  , ifaceTypes     :: IFaceTypes
  , ifaceKinds     :: IFaceKinds
  } deriving (Show)

data Version = Version !Word8 !Word8
    deriving (Eq,Show,Ord)

data FunSymbol = FunSymbol
  { funName :: String
  , funType :: Forall Type
  } deriving (Show)


-- Creation --------------------------------------------------------------------

currentVersion :: Version
currentVersion  = Version 0 1

moduleInterface :: Module -> Interface
moduleInterface m = Interface
  { ifaceNamespace = qualNamespace (modName m)
  , ifaceVersion   = currentVersion
  , ifaceTypes     = mkIFaceTypes ns m
  , ifaceKinds     = mkIFaceKinds ns m
  }
  where
  ns = qualNamespace (modName m)

mkIFaceTypes :: Namespace -> Module -> IFaceTypes
mkIFaceTypes ns m = Map.fromList
                  $ map mkPrimTermFunSymbol (modPrimTerms m)
                 ++ map (mkDeclFunSymbol ns) (modDecls m)

mkPrimTermFunSymbol :: PrimTerm -> (QualName,FunSymbol)
mkPrimTermFunSymbol pt = (qn,sym)
  where
  qn  = primName (primTermName pt)
  sym = FunSymbol
    { funName = mangle qn
    , funType = primTermType pt
    }

mkDeclFunSymbol :: Namespace -> Decl -> (QualName,FunSymbol)
mkDeclFunSymbol ns d = (qn, sym)
  where
  qn  = qualName ns (declName d)
  sym = FunSymbol
    { funName = mangle qn
    , funType = fromJust (declType d) -- dangerous!
    }

mkIFaceKinds :: Namespace -> Module -> IFaceKinds
mkIFaceKinds _ns m = Map.fromList (map mkPrimTypeKind (modPrimTypes m))

mkPrimTypeKind :: PrimType -> (QualName,Kind)
mkPrimTypeKind pt = (primName (primTypeName pt), primTypeKind pt)


-- Query -----------------------------------------------------------------------

-- | Lookup information about a symbol in an interface.
lookupFunSymbol :: QualName -> Interface -> Maybe FunSymbol
lookupFunSymbol qn = Map.lookup qn . ifaceTypes

-- | Lookup the kind of a qualified type name.
lookupKind :: QualName -> Interface -> Maybe Kind
lookupKind qn = Map.lookup qn . ifaceKinds


-- Serialization ---------------------------------------------------------------

interfaceFile :: Interface -> FilePath
interfaceFile iface = joinPath (ifaceNamespace iface) <.> "di"

writeInterface :: Interface -> Dang ()
writeInterface iface =
  withWOBinaryFile (interfaceFile iface) $ \ h ->
    io (S.hPutStr h (runPut (putInterface iface)))

modInterface :: QualName -> FilePath
modInterface qn = joinPath (qualPrefix qn) </> qualSymbol qn <.> "di"

openInterface :: QualName -> Dang Interface
openInterface qn =
  withROBinaryFile (modInterface qn) $ \ h -> do
    bytes <- io (S.hGetContents h)
    case runGet getInterface bytes of
      Left err -> fail err
      Right a  -> return a

putInterface :: Putter Interface
putInterface iface = do
  putVersion    (ifaceVersion iface)
  putNamespace  (ifaceNamespace iface)
  putIFaceTypes (ifaceTypes iface)
  putIFaceKinds (ifaceKinds iface)

getInterface :: Get Interface
getInterface  = Interface
            <$> getVersion
            <*> getNamespace
            <*> getIFaceTypes
            <*> getIFaceKinds

putVersion :: Putter Version
putVersion (Version mj mi) = putWord8 mj >> putWord8 mi

getVersion :: Get Version
getVersion  = Version <$> getWord8 <*> getWord8

putNamespace :: Putter Namespace
putNamespace  = put

getNamespace :: Get Namespace
getNamespace  = get

getIFaceTypes :: Get IFaceTypes
getIFaceTypes  = getMapOf getQualName getFunSymbol

putIFaceTypes :: Putter IFaceTypes
putIFaceTypes  = putMapOf putQualName putFunSymbol

putFunSymbol :: Putter FunSymbol
putFunSymbol fs = put (funName fs) >> putForall putType (funType fs)

getFunSymbol :: Get FunSymbol
getFunSymbol  = FunSymbol <$> get <*> getForall getType

getIFaceKinds :: Get IFaceKinds
getIFaceKinds  = getMapOf getQualName getKind

putIFaceKinds :: Putter IFaceKinds
putIFaceKinds  = putMapOf putQualName putKind
