{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dang.ModuleSystem.Rename (
    rename,
    renameModule,
    renameExpr,

    rnLoc,
    rnModStruct,
    rnModBind,
    rnSchema,
    rnMatch,
    rnDecl,
    rnBind,
  ) where

import Dang.Monad
import Dang.Syntax.AST
import Dang.Syntax.Location
import Dang.ModuleSystem.Env
import Dang.ModuleSystem.Name
import Dang.Unique (SupplyM,withSupply)
import Dang.Utils.Ident (Namespace,packNamespaceLazy)
import Dang.Utils.PP
import Dang.Utils.Panic (panic)

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import           Data.List (nub)
import           Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),ReaderT,ask,local)

type RNModule = Module (Parsed Name)

-- | Rename a top-level module.
renameModule :: PModule -> Dang (Maybe RNModule, Messages)
renameModule Module { .. } = rename (mkNamespace (thing modName)) $
  do n' <- withSupply $ case thing modName of
             PQual ns n -> mkModName (Just ns) n (locRange modName)
             PUnqual n  -> mkModName Nothing   n (locRange modName)

     withNames (envMod (thing modName) [n']) $
       do declEnv <- mergeNames overlapErrors declNames modDecls
          withNames declEnv $
            do RO { .. } <- RN ask
               ds <- traverse rnDecl modDecls
               return Module { modName = n', modDecls = ds, .. }


-- | Rename an expression.
renameExpr :: Namespace -> Expr (Parsed (SrcLoc PName))
           -> Dang (Maybe (Expr (Parsed Name)), Messages)
renameExpr ns e = rename ns (rnExpr e)


-- Monad -----------------------------------------------------------------------

newtype RN a = RN { unRN :: ReaderT RO Dang a
                  } deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)

instance SupplyM RN where
  withSupply f = inBase (withSupply f)


rename :: Namespace -> RN a -> Dang (Maybe a, Messages)
rename ns m =
  do (res,ms) <- collectMessages (runM (unRN m) RO { roNS = ns, roNames = mempty })
     if F.any isError ms
        then return (Nothing,  ms)
        else return (Just res, ms)

data RO = RO { roNS    :: Namespace
             , roNames :: NameMap
             }

pnameToPrefix :: PName -> [L.Text]
pnameToPrefix (PUnqual n)  = [n]
pnameToPrefix (PQual ns n) = ns ++ [n]

mkNamespace :: PName -> Namespace
mkNamespace pn = packNamespaceLazy (pnameToPrefix pn)

-- | Extend the current namespace with the given one.
pushNamespace :: PName -> RN a -> RN a
pushNamespace ns m =
  do ro <- RN ask
     let ns' = roNS ro `T.append` "." `T.append` mkNamespace ns
     RN (local ro { roNS = ns' } (unRN m))

getNamespace :: RN Namespace
getNamespace  = RN (roNS <$> ask)

withNames :: NameMap -> RN a -> RN a
withNames names m =
  do ro     <- RN ask
     names' <- overlapShadows names (roNames ro)
     RN (local ro { roNames = names' } (unRN m))

underMod :: SrcLoc PName -> RN a -> RN a
underMod lmn m = RN $
  do ro <- ask
     local (ro { roNames = openMod (thing lmn) (roNames ro) })
           (unRN m)


-- Naming Environment ----------------------------------------------------------

type NameMap = NameTrie [Name]


-- | Merge name mappings, but add errors when overlap occurs. For the purposes
-- of error reporting, the names in both maps are considered to be defined at
-- the same level.
overlapErrors :: NameMap -> NameMap -> RN NameMap
overlapErrors l r =
  do F.traverse_ (uncurry conflict) (nameList (intersectionWith nubMerge l r))
     return (l `mappend` r)
  where
  nubMerge Nothing Nothing = Nothing
  nubMerge as      bs      = Just (nub (concat (catMaybes [as,bs])))


-- | Merge names, but issue shadowing warnings when overlap occurs. For the
-- purposes of warning reporting, the names on the right are assumed to be the
-- original declarations, with the names on the left being the ones that shadow.
overlapShadows :: NameMap -> NameMap -> RN NameMap
overlapShadows l r =
  do F.traverse_ (uncurry shadows) (nameList (intersectionWith pair l r))
     return (l `shadowing` r)
  where
  pair Nothing Nothing = Nothing
  pair a b             = Just (fromMaybe [] a, fromMaybe [] b)


type GetNames f = f (Parsed (SrcLoc PName)) -> RN NameMap

-- | Merge names, with a monadic implementation of 'mappend'.
mergeNames :: Traversable f
           => (NameMap -> NameMap -> RN NameMap)
           -> (a -> RN NameMap) -> f a -> RN NameMap
mergeNames merge f as = F.foldlM step mempty as
  where
  step acc a =
    do names <- f a
       merge names acc


-- | Introduce a name from a binding site.
newBind :: SrcLoc PName -> RN Name
newBind Located { locValue = PUnqual t, .. } =
  do ns <- getNamespace
     withSupply (mkBinding ns t locRange)
newBind _ = panic "renamer" (text "Qualified name given to `newBind`")

newMod :: SrcLoc PName -> RN Name
newMod Located { locValue = PUnqual t, .. } =
  do ns <- getNamespace
     withSupply (mkModName (Just [L.fromStrict ns]) t locRange)
newMod _ = panic "renamer" (text "Qualified name given to `newMod`")

newParam :: ParamSource -> SrcLoc PName -> RN Name
newParam d Located { locValue = PUnqual t, .. } =
     withSupply (mkParam d t locRange)
newParam _ _ = panic "renamer" (text "Qualified name given to `newParam`")

-- | Introduce names for the given binding.
bindName :: GetNames Bind
bindName Bind { .. } =
  do name <- newBind bName
     return (envDecl (thing bName) [name])

-- | Introduce names for 
--
-- XXX: do signatures need to bind names?
letDeclNames :: GetNames LetDecl
letDeclNames (LDBind loc b) = withLoc loc (bindName b)
letDeclNames LDSig{}        = return mempty

-- | Introduce names for all bindings within a declaration. NOTE: this will
-- traverse into module definitions, introducing names for visible bindings.
declNames :: GetNames Decl
declNames (DBind  l b)     = withLoc l (bindName b)
declNames (DModBind l mb)  = withLoc l (modBindNames mb)
declNames (DData l d)      = withLoc l (dataNames d)
declNames DSig{}           = return mempty

-- | Names introduced by a module binding.
modBindNames :: GetNames ModBind
modBindNames ModBind { .. } =
  addLoc mbName $ \ ns -> pushNamespace ns $
    do n'    <- newMod mbName
       names <- modExprNames mbExpr
       return (envMod ns [n'] `mappend` qualify (pnameToPrefix ns) names)

-- | The names introduced by a data declaration.
dataNames :: GetNames Data
dataNames Data { .. } =
  do tyName   <- newBind dName
     conNames <- traverse constrName dConstrs
     return $ mconcat
            $ envType (thing dName) [tyName] : conNames

-- | Names introduced by a constructor.
constrName :: GetNames Constr
constrName Constr { .. } = withLoc cMeta $
  do conName <- newBind cName
     return (envDecl (thing cName) [conName])

-- | Names defined by a module expression.
modExprNames :: GetNames ModExpr
modExprNames (MEStruct l ms)       = withLoc l (modStructNames ms)
modExprNames (MEConstraint l me _) = withLoc l (modExprNames me)
modExprNames _                     = return mempty

-- | Names defined by a module structure.
modStructNames :: GetNames ModStruct
modStructNames ModStruct { .. } =
  mergeNames overlapErrors declNames msElems

-- | Names defined by a pattern.
patNames :: ParamSource -> GetNames Pat
patNames d = go
  where
  go :: GetNames Pat
  go (PCon l _ ps) = withLoc l (mergeNames overlapErrors go ps)
  go (PWild _)     = return mempty
  go (PVar l ln)   = withLoc l $
    do n <- newParam d ln
       return (envDecl (thing ln) [n])


-- Renaming --------------------------------------------------------------------

type Rename f = f (Parsed (SrcLoc PName)) -> RN (f (Parsed Name))

rnLoc :: (a -> RN b) -> SrcLoc a -> RN b
rnLoc f Located { .. } = withLoc locRange (f locValue)

getName :: (PName -> NameMap -> Maybe [Name]) -> Doc
        -> PName -> RN Name
getName lkp ty pn =
  do RO { .. } <- RN ask
     case lkp pn roNames of
       Just []  -> unknown ty pn
       Just [n] -> return n
       Just ns  -> conflict pn ns
       Nothing  -> unknown ty pn

rnEName, rnTName, rnMName :: PName -> RN Name
rnEName  = getName lookupDecl (text "declaration")
rnTName  = getName lookupDecl (text "type")
rnMName  = getName lookupMod  (text "module")


-- Modules ---------------------------------------------------------------------

-- | Qualify all of the declarations in the struct.
rnModStruct :: Rename ModStruct
rnModStruct (ModStruct l ds) = withLoc l (ModStruct l <$> traverse rnDecl ds)

-- | Rename a declaration.
rnDecl :: Rename Decl
rnDecl (DBind l b)     = withLoc l (DBind    l <$> rnBind b)
rnDecl (DModBind l mb) = withLoc l (DModBind l <$> rnModBind mb)
rnDecl (DSig l s)      = panic "rename" $ text "Unexpected signature found"
                                     $$ text (show s)

-- | Rename a module binding.
rnModBind :: Rename ModBind
rnModBind ModBind { .. } = pushNamespace (thing mbName) $
  do n' <- rnLoc rnMName mbName
     e' <- underMod mbName (rnModExpr (FromBind n') mbExpr)
     return ModBind { mbName = n', mbExpr = e', .. }

rnModExpr :: ParamSource -> Rename ModExpr
rnModExpr d = go
  where
  go (MEName l n)           = withLoc l (MEName   l <$> rnLoc rnMName n)
  go (MEApp l f x)          = withLoc l (MEApp    l <$> go f <*> go x)
  go (MEStruct l s)         = withLoc l (MEStruct l <$> rnModStruct s)
  go (MEFunctor l a sig e)  = withLoc l $
    do p    <- newParam d a
       sig' <- rnModType sig
       withNames (envMod (thing a) [p]) (MEFunctor l p sig' <$> go e)
  go (MEConstraint l n sig) = error "rnModExpr"


rnModType :: Rename ModType
rnModType (MTVar l n)          = withLoc l (MTVar l <$> rnLoc rnMName n)
rnModType (MTSig l sig)        = error "rnModType"
rnModType (MTFunctor l n ty e) = error "rnModType"



-- Expressions -----------------------------------------------------------------

-- | Rename a binding. This assumes that new names have already been introduced
-- externally.
rnBind :: Rename Bind
rnBind Bind { .. } = withLoc bMeta $
  do n'   <- rnLoc rnEName bName
     pats <- mergeNames overlapErrors (patNames (FromBind n')) bParams
     withNames pats $
       do ps' <- traverse rnPat bParams
          b'  <- rnExpr bBody
          return Bind { bName   = n'
                      , bParams = ps'
                      , bBody   = b'
                      , .. }

rnMatch :: ParamSource -> Rename Match
rnMatch d = go
  where
  go (MSplit l a b) = withLoc l (MSplit l <$> go a <*> go b)
  go (MFail l)      = return (MFail l)
  go (MExpr l e)    = withLoc l (MExpr  l <$> rnExpr e)
  go (MPat l p m)   = withLoc l $
    do names <- patNames d p
       withNames names (MPat l <$> rnPat p <*> go m)

rnPat :: Rename Pat
rnPat (PCon l c ps) = withLoc l (PCon l <$> rnLoc rnEName c <*> traverse rnPat ps)
rnPat (PWild l)     = pure (PWild l)
rnPat (PVar l n)    = withLoc l (PVar l <$> rnLoc rnEName n)

-- | Rename an expression.
rnExpr :: Rename Expr
rnExpr (EVar l pn)   = withLoc l (EVar l <$> rnLoc rnEName pn)
rnExpr (ECon l pn)   = withLoc l (ECon l <$> rnLoc rnEName pn)
rnExpr (EApp l f xs) = withLoc l (EApp l <$> rnExpr f <*> traverse rnExpr xs)

rnExpr (EAbs l m)    = withLoc l $
  do loc <- askLoc
     EAbs l <$> rnMatch (FromLambda loc) m

rnExpr (ELit l m)    = withLoc l (ELit l <$> rnLit m)
rnExpr (ELet l ds e) = withLoc l $
  do names <- mergeNames overlapErrors letDeclNames ds
     withNames names (ELet l <$> traverse rnLetDecl ds <*> rnExpr e)

rnLit :: Rename Literal
rnLit (LInt l i b) = pure (LInt l i b)



rnLetDecl :: Rename LetDecl
rnLetDecl (LDBind l b) = withLoc l (LDBind l <$> rnBind b)
rnLetDecl (LDSig l s)  = panic "renamer" (text "signature found in let binding")


-- Types -----------------------------------------------------------------------

rnSchema :: Rename Schema
rnSchema  = error "rnSchema"


-- Errors/Warnings -------------------------------------------------------------

conflict :: PName -> [Name] -> RN Name
conflict d ns =
  do addError ErrRnOverlap (hang msg 2 (vcat (map ppNameOrigin ns)))
     return (head ns)
  where
  msg = pp d <+> text "is defined in multiple places:"

shadows :: PName -> ([Name],[Name]) -> RN ()
shadows d (new,old)
  | null new || null old = panic "renamer" (text "Invalid use of `shadows`")
  | otherwise            =
    withLoc (getLoc (head new)) (addWarning WarnRnShadowing msg)
  where
  msg = pp d <> char ',' <+> ppNameOrigin (head old)
    <+> text "is shadowed by"
    <+> pp d <> char ',' <+> ppNameOrigin (head new)

-- | Invent a name for a parsed name, and record an error about a missing
-- identifier.
unknown :: Doc -> PName -> RN Name
unknown ty d =
  do addError ErrRnUnknown (ty <+> text "not in scope:" <+> pp d)
     loc <- askLoc
     ns  <- getNamespace
     inBase (withSupply (mkUnknown (Declaration (ModInfo ns)) d loc))
