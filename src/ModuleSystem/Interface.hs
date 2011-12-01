{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ModuleSystem.Interface where

import Dang.IO (withROBinaryFile,withWOBinaryFile)
import Dang.Monad (Dang,io)
import ModuleSystem.Types (UsedName(..))
import QualName
import Syntax.AST
import TypeChecker.Types

import Control.Applicative ((<$>),(<*>))
import Data.Serialize
    (runPut,Putter,putMapOf,putListOf,runGet,Get,getMapOf,getListOf)
import MonadLib (BaseM)
import System.FilePath ((</>))
import qualified Data.Map as Map
import qualified Data.ByteString as S


-- Interface Interaction -------------------------------------------------------

type Lookup i a = QualName -> i -> Maybe a

class HasInterface i where
  lookupSymbol   :: Lookup i Symbol
  lookupData     :: Lookup i DataDecl
  lookupPrimType :: Lookup i PrimType
  lookupPrimTerm :: Lookup i PrimTerm


-- Defined Symbols -------------------------------------------------------------

-- | A typed symbol.
data Symbol = Symbol
  { symExternal :: Name -- ^ External name
  , symInternal :: Name -- ^ Object symbol name, fully mangled
  , symType     :: Scheme
  } deriving (Show)


-- Interfaces ------------------------------------------------------------------

type NameMap = Map.Map QualName

-- | This represents the interface for a single module.  An interface contains
-- all exported symbols and types.
data Interface = Interface
  { ifaceName      :: QualName
  , ifaceSymbols   :: NameMap Symbol
  , ifaceDatas     :: NameMap DataDecl
  , ifacePrimTypes :: NameMap PrimType
  , ifacePrimTerms :: NameMap PrimTerm
  } deriving (Show)

symbolNames :: Interface -> [UsedName]
symbolNames  = map UsedTerm . Map.keys . ifaceSymbols

dataNames :: Interface -> [UsedName]
dataNames iface = Map.foldlWithKey step [] (ifaceDatas iface)
  where
  ns           = qualNamespace (ifaceName iface)
  step us qn d = UsedType qn : cs ++ us
    where
    cs = map (UsedTerm . qualName ns . constrName) (forallData (dataConstrs d))

primTypeNames :: Interface -> [UsedName]
primTypeNames  = map UsedType . Map.keys . ifacePrimTypes

primTermNames :: Interface -> [UsedName]
primTermNames  = map UsedTerm . Map.keys . ifacePrimTerms

ifaceNames :: Interface -> [UsedName]
ifaceNames iface = concat
  [ symbolNames   iface
  , dataNames     iface
  , primTypeNames iface
  , primTermNames iface
  ]

instance HasInterface Interface where
  lookupSymbol   qn = Map.lookup qn . ifaceSymbols
  lookupData     qn = Map.lookup qn . ifaceDatas
  lookupPrimType qn = Map.lookup qn . ifacePrimTypes
  lookupPrimTerm qn = Map.lookup qn . ifacePrimTerms


-- Interface Sets --------------------------------------------------------------

newtype InterfaceSet = InterfaceSet
  { getInterfaces :: NameMap Interface
  } deriving (Show)

lookupInterface :: QualName -> InterfaceSet -> Maybe Interface
lookupInterface qn (InterfaceSet iset) = Map.lookup qn iset

liftInterfaceSet :: Lookup Interface a -> Lookup InterfaceSet a
liftInterfaceSet k qn iset = do
  m     <- qualModule qn
  iface <- lookupInterface m iset
  k qn iface

instance HasInterface InterfaceSet where
  lookupSymbol   = liftInterfaceSet lookupSymbol
  lookupData     = liftInterfaceSet lookupData
  lookupPrimType = liftInterfaceSet lookupPrimType
  lookupPrimTerm = liftInterfaceSet lookupPrimTerm


-- Interface Serialization -----------------------------------------------------

modInterface :: QualName -> FilePath
modInterface qn = foldr (</>) (qualSymbol qn) (qualPrefix qn)

writeInterface :: BaseM m Dang => Interface -> m ()
writeInterface iface =
  withWOBinaryFile (modInterface (ifaceName iface)) $ \ h ->
    io (S.hPutStr h (runPut (putInterface iface)))

readInterface :: BaseM m Dang => QualName -> m Interface
readInterface qn =
  withROBinaryFile (modInterface qn) $ \ h -> do
    bytes <- io (S.hGetContents h)
    case runGet getInterface bytes of
      Left err    -> fail err
      Right iface -> return iface

putInterface :: Putter Interface
putInterface iface = do
  putQualName            (ifaceName iface)
  putNameMap putSymbol   (ifaceSymbols iface)
  putNameMap putData     (ifaceDatas iface)
  putNameMap putPrimType (ifacePrimTypes iface)
  putNameMap putPrimTerm (ifacePrimTerms iface)

getInterface :: Get Interface
getInterface  = Interface
            <$> getQualName
            <*> getNameMap getSymbol
            <*> getNameMap getData
            <*> getNameMap getPrimType
            <*> getNameMap getPrimTerm

putNameMap :: Putter a -> Putter (NameMap a)
putNameMap  = putMapOf putQualName

getNameMap :: Get a -> Get (NameMap a)
getNameMap  = getMapOf getQualName

putSymbol :: Putter Symbol
putSymbol sym = do
  putName           (symExternal sym)
  putName           (symInternal sym)
  putForall putType (symType sym)

getSymbol :: Get Symbol
getSymbol  = Symbol <$> getName <*> getName <*> getForall getType

putData :: Putter DataDecl
putData d = do
  putName                         (dataName d)
  putForall (putListOf putConstr) (dataConstrs d)

getData :: Get DataDecl
getData  = DataDecl <$> getName <*> getForall (getListOf getConstr)

putConstr :: Putter Constr
putConstr c = do
  putName (constrName c)
  putType (constrType c)

getConstr :: Get Constr
getConstr  = Constr <$> getName <*> getType

putPrimType :: Putter PrimType
putPrimType p = do
  putName (primTypeName p)
  putKind (primTypeKind p)

getPrimType :: Get PrimType
getPrimType  = PrimType <$> getName <*> getType

putPrimTerm :: Putter PrimTerm
putPrimTerm p = do
  putName           (primTermName p)
  putForall putType (primTermType p)

getPrimTerm :: Get PrimTerm
getPrimTerm  = PrimTerm <$> getName <*> getForall getType
