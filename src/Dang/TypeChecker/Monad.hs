{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Dang.TypeChecker.Monad (
    -- * Type Checking Monad
    TC()
  , runTC
  , tcPanic

    -- ** Unification
  , unify
  , applySubst

    -- ** Variables
  , freshVar
  , freshVarFromTParam

    -- ** Types
  , freshInst
  ) where

import Dang.Monad
import Dang.TypeChecker.Env
import Dang.TypeChecker.Types
import Dang.TypeChecker.Unify
import Dang.Utils.Panic
import Dang.Utils.Pretty

import Control.Applicative ( Alternative )
import Control.Monad ( MonadPlus, mzero, when )
import Control.Monad.Fix ( MonadFix )
import MonadLib ( BaseM(..), runM, ReaderT, StateT, get, set )


newtype TC a = TC { unTC :: ReaderT RO (StateT RW Dang) a }
    deriving (Functor,Applicative,Alternative,Monad,MonadFix,MonadPlus)

runTC :: TC a -> Dang a
runTC m =
  do (a,_) <- runM (unTC m) emptyRO emptyRW
     return a

tcPanic :: PPDoc -> a
tcPanic  = panic "typechecker"

instance BaseM TC Dang where
  inBase m = TC (inBase m)


-- Read-only Environment -------------------------------------------------------

newtype RO = RO { roEnv :: Env }

emptyRO :: RO
emptyRO  = RO { roEnv = emptyEnv }


-- Read/Write State ------------------------------------------------------------

data RW = RW { rwVarEnv :: VarEnv
             , rwFresh  :: !Int
             }

emptyRW :: RW
emptyRW  = RW { rwVarEnv = mempty, rwFresh = 0 }


-- Primitive Operations --------------------------------------------------------

-- | Unify two types, modifying the internal substitution.
unify :: Type -> Type -> TC ()
unify l r = do
  rw <- TC get
  case mgu l r (rwVarEnv rw) of
    Right env' -> TC (set rw { rwVarEnv = env' })
    Left err   -> addErr err

-- | Apply the substitution to a type.
applySubst :: Types t => t -> TC t
applySubst ty =
  do rw <- TC get
     case zonk (rwVarEnv rw) ty of
       Right ty' -> return ty'
       Left err  -> do addErr err
                       mzero

-- | The next free variable index.
nextIndex :: TC Int
nextIndex  =
  do rw <- TC get
     TC (set rw { rwFresh = rwFresh rw + 1 })
     return (rwFresh rw)

-- | Freshen a type parameter.
freshTParam :: TParam -> TC TParam
freshTParam p =
  do ix <- nextIndex
     return (p { tpIndex = ix })

-- | Generate a new type variable, given a @TParam@ as a template.
freshVarFromTParam :: TParam -> TC Type
freshVarFromTParam p = TVar `fmap` freshTParam p

-- | Generate fresh type variables, with the given kind.
freshVar :: Kind -> TC Type
freshVar k =
  freshVarFromTParam TParam { tpName = Nothing, tpIndex = 0, tpKind = k }

-- | Freshly instantiate a @Schema@, returning the fresh type.
freshInst :: Schema -> TC Type
freshInst ty =
  do ps' <- mapM freshTParam (sParams ty)
     inst (map TVar ps') ty

-- | Instantiate a @Schema@.
inst :: [Type] -> Schema -> TC Type
inst tys s =
  do when (length tys /= length (sParams s)) $
       do addErr $ hang (text "invalid schema instantiation:")
                      2 (vcat [ text "       Schema:" <+> pp s
                              , text "Instantiation:" <+> fsep (commas (map pp tys)) ])
          mzero

     -- XXX emit goals

     ty <- applySubst (sType s)
     let u = bindBounds (zip (sParams s) tys)
     case zonk u ty of
       Right ty' -> return ty'
       Left _    -> tcPanic (text "occurs check failed during instantiation")
