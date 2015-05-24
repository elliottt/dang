{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

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

    -- ** Constraints
  , Goal, Goal'(..)
  , getGoals, setGoals
  , newGoal, newGoals
  , emitGoal, emitGoals

    -- ** Types
  , freshInst
  , inst
  ) where

import Dang.Monad
import Dang.TypeChecker.Env
import Dang.TypeChecker.Subst
import Dang.TypeChecker.Types
import Dang.TypeChecker.Unify
import Dang.Utils.Location (Located,at,unLoc)
import Dang.Utils.Panic
import Dang.Utils.Pretty

import Control.Applicative ( Alternative )
import Control.Monad (MonadPlus,mzero,when,unless)
import Control.Monad.Fix ( MonadFix )
import GHC.Generics (Generic)
import MonadLib (BaseM(..),runM,ReaderT,StateT,get,set,ask,local)


-- TC Monad --------------------------------------------------------------------

newtype TC a = TC { unTC :: ReaderT RO (StateT RW Dang) a }
    deriving (Functor,Applicative,Alternative,Monad,MonadFix,MonadPlus)

runTC :: TC a -> Dang a
runTC m =
  do (a,RW { .. }) <- runM (unTC m) emptyRO emptyRW

     unless (null rwGoals) $ addErr $
       hang (text "unsolved goals remaining:")
          2 (vcat (map ppGoal rwGoals))

     return a

tcPanic :: PPDoc -> a
tcPanic  = panic "typechecker"

instance BaseM TC Dang where
  inBase m = TC (inBase m)


-- Read-only Environment -------------------------------------------------------

newtype RO = RO { roEnv :: Env }

emptyRO :: RO
emptyRO  = RO { roEnv = emptyEnv }

-- | Retrieve the type environment.
getEnv :: TC Env
getEnv  = TC (roEnv `fmap` ask)

-- | Shadow the type environment.
withEnv :: Env -> TC a -> TC a
withEnv env (TC m) = TC $
  do RO { .. } <- ask
     local RO { roEnv = env, .. } m


-- Read/Write State ------------------------------------------------------------

data RW = RW { rwSubst :: !Subst
             , rwFresh :: !Int
             , rwGoals :: [Goal] }

emptyRW :: RW
emptyRW  = RW { rwSubst = mempty, rwFresh = 0, rwGoals = [] }

-- | Get the current substitution.
getSubst :: TC Subst
getSubst  = TC (rwSubst `fmap` get)

-- | Set the current substitution.
setSubst :: Subst -> TC ()
setSubst s = TC $
  do RW { .. } <- get
     set RW { rwSubst = s, .. }

-- | Compose this substitution with the current one.
extendSubst :: Subst -> TC ()
extendSubst su' =
  do su <- getSubst
     setSubst $! su @@ su'

-- | Remove all goals from the current context.
getGoals :: TC [Goal]
getGoals  = TC $
  do RW { .. } <- get
     set RW { rwGoals = [], .. }
     return rwGoals

-- | Replace the current set of goals with this one.
setGoals :: [Goal] -> TC ()
setGoals gs = TC $
  do RW { .. } <- get
     set RW { rwGoals = gs, .. }

-- | Collect the goals produced by an action.
collectGoals :: TC a -> TC (a,[Goal])
collectGoals body =
  do gs  <- getGoals
     a   <- body
     gs' <- getGoals
     setGoals gs
     return (a,gs')


-- | A goal, with an attached location.
type Goal = Located Goal'

data Goal' = Goal { gProp :: Prop
                  } deriving (Show,Generic)

instance Types Goal'

ppGoal :: Goal -> PPDoc
ppGoal lg = pp (gProp (unLoc lg))

newGoal :: Prop -> TC Goal
newGoal gProp =
  do loc <- askLoc
     return (Goal { .. } `at` loc)

newGoals :: [Prop] -> TC [Goal]
newGoals ps =
  do loc <- askLoc
     return (map (\ gProp -> Goal { .. } `at` loc) ps)

emitGoal :: Goal -> TC ()
emitGoal g =
  do gs <- getGoals
     setGoals (g:gs)

emitGoals :: [Goal] -> TC ()
emitGoals gs' =
  do gs <- getGoals
     setGoals (gs' ++ gs)


-- Primitive Operations --------------------------------------------------------

-- | Unify two types, modifying the internal substitution.
unify :: Type -> Type -> TC ()
unify l r =
  do su <- getSubst
     case mgu (apply su l) (apply su r) of
       Right su' -> extendSubst su'
       Left err  -> addErr err

-- | Apply the substitution to a type.
applySubst :: Types t => t -> TC t
applySubst ty =
  do su <- getSubst
     return $! apply su ty

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

-- | Freshly instantiate a @Schema@, returning the constraints and type.
freshInst :: Schema -> TC ([Prop],Type)
freshInst ty =
  do ps' <- mapM freshTParam (sParams ty)
     inst ty (map TVar ps')

-- | Explicit instantiation of a @Schema@, returning the instantiated
-- constraints and type.
inst :: Schema -> [Type] -> TC ([Prop],Type)
inst s@Forall { .. } tys =
  do when (length tys /= length sParams) $
       do addErr $ hang (text "invalid schema instantiation:")
                      2 (vcat [ text "       Schema:" <+> pp s
                              , text "Instantiation:" <+> fsep (commas (map pp tys)) ])
          mzero

     ty <- applySubst sType
     let su = listBoundSubst (zip sParams tys)
     return (apply su sProps, apply su ty)
