{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.Monad where

import Dang.Monad
import Interface
import QualName
import TypeChecker.Types
import TypeChecker.Unify

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Map as Map


newtype TC a = TC { unTC :: ReaderT RO (StateT RW Dang) a }
    deriving (Functor,Applicative,Monad,MonadFix)

runTC :: InterfaceSet -> TC a -> Dang a
runTC iset (TC m) = fst `fmap` runStateT emptyRW (runReaderT (emptyRO iset) m)

instance BaseM TC Dang where
  inBase = TC . inBase

instance ExceptionM TC SomeException where
  raise = TC . raise

instance RunExceptionM TC SomeException where
  try = TC . try . unTC

data RW = RW
  { rwSubst :: Subst
  , rwIndex :: !Index
  }

emptyRW :: RW
emptyRW  = RW
  { rwSubst = emptySubst
  , rwIndex = 0
  }

type Assumps = Map.Map QualName Type

data RO = RO
  { roInterfaceSet :: InterfaceSet
  , roTypeBindings :: Map.Map QualName Type
  , roKindBindings :: Map.Map QualName Kind
  }

emptyRO :: InterfaceSet -> RO
emptyRO iset = RO
  { roInterfaceSet = iset
  , roTypeBindings = Map.empty
  , roKindBindings = Map.empty
  }

roBindKind :: QualName -> Kind -> RO -> RO
roBindKind qn k ro = ro { roKindBindings = Map.insert qn k (roKindBindings ro) }

roBindKinds :: [(QualName,Kind)] -> RO -> RO
roBindKinds binds ro =
  ro { roKindBindings = Map.union (Map.fromList binds) (roKindBindings ro) }

roFindKind :: QualName -> RO -> Maybe Kind
roFindKind qn ro = fromISet `mplus` fromBinding
  where
  fromISet    = lookupKind qn (roInterfaceSet ro)
  fromBinding = Map.lookup qn (roKindBindings ro)

roBindType :: QualName -> Kind -> RO -> RO
roBindType qn k ro = ro { roTypeBindings = Map.insert qn k (roTypeBindings ro) }

roBindTypes :: [(QualName,Type)] -> RO -> RO
roBindTypes binds ro =
  ro { roTypeBindings = Map.union (Map.fromList binds) (roTypeBindings ro) }

roFindScheme :: QualName -> RO -> Maybe (Forall Type)
roFindScheme qn ro = fromISet `mplus` fromBinding
  where
  fromISet    = funType `fmap` lookupFunSymbol qn (roInterfaceSet ro)
  fromBinding = Forall [] `fmap` Map.lookup qn (roTypeBindings ro)


-- Primitive Operations --------------------------------------------------------

-- | Unify two types, modifying the internal substitution.
unify :: Type -> Type -> TC ()
unify l r = do
  u <- getSubst
  extSubst =<< mgu (apply u l) (apply u r)

-- | Get the current substitution.
getSubst :: TC Subst
getSubst  = rwSubst `fmap` TC get

-- | Extend the current substitution by merging in the given one.
extSubst :: Subst -> TC ()
extSubst u = do
  rw <- TC get
  TC (set rw { rwSubst = u @@ rwSubst rw })

-- | Apply the substitution to a type.
applySubst :: Type -> TC Type
applySubst ty = do
  rw <- TC get
  return (apply (rwSubst rw) ty)

-- | Reset the index base for a type-checking operation.
withVarIndex :: Index -> TC a -> TC a
withVarIndex i' m = do
  rw <- TC get
  TC (set rw { rwIndex = i' })
  a  <- m
  TC (set rw { rwIndex = rwIndex rw })
  return a

-- | Generate fresh type variables, with the given kind.
freshVar :: Kind -> TC Type
freshVar k = do
  rw <- TC get
  TC (set rw { rwIndex = rwIndex rw + 1 })
  return (TVar (rwIndex rw) (TParam "" k))

data UnboundIdentifier = UnboundIdentifier QualName
    deriving (Show,Typeable)

instance Exception UnboundIdentifier

lookupRO :: (QualName -> RO -> Maybe a) -> QualName -> TC a
lookupRO find qn = do
  ro <- TC ask
  case find qn ro of
    Nothing -> raiseE (UnboundIdentifier qn)
    Just a  -> return a

findKind :: QualName -> TC Kind
findKind  = applySubst <=< lookupRO roFindKind

findType :: QualName -> TC Type
findType qn = do
  Forall ps ty <- lookupRO roFindScheme qn
  vars         <- mapM (freshVar . paramKind) ps
  applySubst =<< inst vars ty

modifyRO :: (RO -> RO) -> TC a -> TC a
modifyRO f m = TC $ do
  ro <- ask
  local (f ro) (unTC m)

addKindBinding :: QualName -> Kind -> TC a -> TC a
addKindBinding qn k = modifyRO (roBindKind qn k)

addKindBindings :: [(QualName,Kind)] -> TC a -> TC a
addKindBindings binds = modifyRO (roBindKinds binds)

addTypeBinding :: QualName -> Type -> TC a -> TC a
addTypeBinding qn ty = modifyRO (roBindType qn ty)

addTypeBindings :: [(QualName,Type)] -> TC a -> TC a
addTypeBindings binds = modifyRO (roBindTypes binds)
