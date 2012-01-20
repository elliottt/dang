{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ModuleSystem.Interface where

import Dang.IO (withROBinaryFile,withWOBinaryFile)
import Dang.Monad (Dang,io)
import ModuleSystem.Export (Export(..))
import ModuleSystem.Types (UsedName(..))
import QualName
import Syntax.AST
    (DataDecl(..),ConstrGroup(..),Constr(..),PrimType(..),PrimTerm(..))
import TypeChecker.Types
    (Kind,Scheme,Forall(..),putForall,getForall,putType,getType,putKind,getKind)

import Control.Applicative (pure,(<$>),(<*>))
import Data.Serialize
    (runPut,Putter,putMapOf,putListOf,runGet,Get,getMapOf,getListOf,get,put)
import MonadLib (BaseM)
import System.FilePath ((</>),(<.>))
import qualified Data.Map as Map
import qualified Data.ByteString as S


-- Interface Interaction -------------------------------------------------------

type Lookup  i a = QualName -> i -> Maybe a
type Listing i a = i -> [(QualName,a)]

class HasInterface i where
  lookupSymbol   :: Lookup  i Symbol
  getSymbols     :: Listing i Symbol
  lookupData     :: Lookup  i DataDecl
  getDatas       :: Listing i DataDecl
  lookupPrimType :: Lookup  i PrimType
  getPrimTypes   :: Listing i PrimType
  lookupPrimTerm :: Lookup  i PrimTerm
  getPrimTerms   :: Listing i PrimTerm

getTypes :: HasInterface i => Listing i Scheme
getTypes i = concat
  [ [ (qn, symType sym)     | (qn,sym) <- getSymbols i   ]
  , [ (qn, primTermType pt) | (qn,pt)  <- getPrimTerms i ]
    -- XXX add constructors
  ]

getKinds :: HasInterface i => Listing i Kind
getKinds i = concat
  [ [ (qn, primTypeKind pt) | (qn,pt)  <- getPrimTypes i ]
    -- XXX add data types
  ]


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

emptyInterface :: QualName -> Interface
emptyInterface qn = Interface
  { ifaceName      = qn
  , ifaceSymbols   = Map.empty
  , ifaceDatas     = Map.empty
  , ifacePrimTypes = Map.empty
  , ifacePrimTerms = Map.empty
  }

symbolNames :: Interface -> [UsedName]
symbolNames  = map UsedTerm . Map.keys . ifaceSymbols

dataNames :: Interface -> [UsedName]
dataNames iface = Map.foldlWithKey step [] (ifaceDatas iface)
  where
  ns           = qualNamespace (ifaceName iface)
  step us qn d = UsedType qn : cs ++ us
    where
    cs = concatMap (constrGroupNames ns . forallData) (dataGroups d)

constrGroupNames :: Namespace -> ConstrGroup -> [UsedName]
constrGroupNames ns cg = map (constrNames ns) (groupConstrs cg)

constrNames :: Namespace -> Constr -> UsedName
constrNames ns = UsedTerm . qualName ns . constrName

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
  getSymbols        = Map.toList    . ifaceSymbols

  lookupData     qn = Map.lookup qn . ifaceDatas
  getDatas          = Map.toList    . ifaceDatas

  lookupPrimType qn = Map.lookup qn . ifacePrimTypes
  getPrimTypes      = Map.toList    . ifacePrimTypes

  lookupPrimTerm qn = Map.lookup qn . ifacePrimTerms
  getPrimTerms      = Map.toList    . ifacePrimTerms


-- Interface Sets --------------------------------------------------------------

newtype InterfaceSet = InterfaceSet
  { getInterfaces :: NameMap Interface
  } deriving (Show)

emptyInterfaceSet :: InterfaceSet
emptyInterfaceSet  = InterfaceSet Map.empty

addInterface :: Interface -> InterfaceSet -> InterfaceSet
addInterface iface (InterfaceSet iset) =
  InterfaceSet (Map.insert (ifaceName iface) iface iset)

lookupInterface :: QualName -> InterfaceSet -> Maybe Interface
lookupInterface qn (InterfaceSet iset) = Map.lookup qn iset

liftLookup :: Lookup Interface a -> Lookup InterfaceSet a
liftLookup k qn iset = do
  m     <- qualModule qn
  iface <- lookupInterface m iset
  k qn iface

liftListing :: Listing Interface a -> Listing InterfaceSet a
liftListing k = concatMap k . Map.elems . getInterfaces


instance HasInterface InterfaceSet where
  lookupSymbol   = liftLookup  lookupSymbol
  getSymbols     = liftListing getSymbols
  lookupData     = liftLookup  lookupData
  getDatas       = liftListing getDatas
  lookupPrimType = liftLookup  lookupPrimType
  getPrimTypes   = liftListing getPrimTypes
  lookupPrimTerm = liftLookup  lookupPrimTerm
  getPrimTerms   = liftListing getPrimTerms


-- Interface Serialization -----------------------------------------------------

modInterface :: QualName -> FilePath
modInterface qn = foldr (</>) (qualSymbol qn <.> "di") (qualPrefix qn)

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
  putName                              (dataName d)
  put                                  (dataArity d)
  putKind                              (dataKind d)
  putListOf (putForall putConstrGroup) (dataGroups d)

getData :: Get DataDecl
getData  = DataDecl
       <$> getName
       <*> get
       <*> getKind
       <*> pure Public
       <*> getListOf (getForall getConstrGroup)

putConstrGroup :: Putter ConstrGroup
putConstrGroup cg = do
  putListOf putType   (groupArgs cg)
  putListOf putConstr (groupConstrs cg)

getConstrGroup :: Get ConstrGroup
getConstrGroup  = ConstrGroup <$> getListOf getType <*> getListOf getConstr

putConstr :: Putter Constr
putConstr c = do
  putName           (constrName c)
  putListOf putType (constrFields c)

getConstr :: Get Constr
getConstr  = Constr <$> getName <*> pure Public <*> getListOf getType

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
