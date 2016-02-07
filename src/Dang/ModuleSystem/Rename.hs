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
import Dang.ModuleSystem.Name (Name,mkModName,mkUnknown,mkBinding)
import Dang.Unique (SupplyM,withSupply)
import Dang.Utils.Ident (Namespace,packNamespaceLazy)
import Dang.Utils.PP
import Dang.Utils.Panic (panic)

import           Control.Applicative (Alternative(..))
import           Control.Lens (Lens',over,view)
import           Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           MonadLib (runM,BaseM(..),ReaderT,ask,local)


-- | Rename a top-level module.
renameModule :: Module PName -> Dang (Either [Message] (Module Name))
renameModule Module { .. } = rename (mkNamespace (thing modName)) $
  do n' <- withSupply $ case thing modName of
             PQual ns n -> mkModName (Just ns) n (locRange modName)
             PUnqual n  -> mkModName Nothing   n (locRange modName)

     withNames (envMod (thing modName) [n']) $
       do declEnv <- mergeNames overlapErrors declNames modDecls
          withNames declEnv $
            do RO { .. } <- RN ask
               ds <- traverse rnDecl modDecls
               return Module { modName = n' `at` modName, modDecls = ds }


-- | Rename an expression.
renameExpr :: Namespace -> Expr PName -> Dang (Either [Message] (Expr Name))
renameExpr ns e = rename ns (rnExpr e)


-- Monad -----------------------------------------------------------------------

newtype RN a = RN { unRN :: ReaderT RO Dang a
                  } deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance BaseM RN Dang where
  inBase m = RN (inBase m)

instance SupplyM RN where
  withSupply f = inBase (withSupply f)


rename :: Namespace -> RN a -> Dang (Either [Message] a)
rename ns m =
  do (res,ms) <- collectMessages (runM (unRN m) RO { roNS = ns, roNames = mempty })
     if any isError ms
        then return (Left ms)
        else do putMessages ms
                return (Right res)

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

underMod :: Located PName -> RN a -> RN a
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


type GetNames f = f PName -> RN NameMap

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
newName :: Located PName -> RN Name
newName Located { locValue = PUnqual t, .. } =
  do ns <- getNamespace
     withSupply (mkBinding ns t locRange)
newName _ = panic "renamer" (text "Qualified name given to `newName`")

-- | Introduce names for the given binding.
bindName :: GetNames Bind
bindName Bind { .. } =
  do name <- newName bName
     return (envDecl (thing bName) [name])

-- | Introduce names for 
letDeclNames :: GetNames LetDecl
letDeclNames (LDBind b) = bindName b
letDeclNames (LDLoc l)  = addLoc l letDeclNames
letDeclNames LDSig{}    = return mempty

-- | Introduce names for all bindings within a declaration. NOTE: this will
-- traverse into module definitions, introducing names for visible bindings.
declNames :: GetNames Decl
declNames (DBind b)     = bindName b
declNames (DModBind mb) = modBindNames mb
declNames (DData d)     = dataNames d
declNames (DLoc dl)     = addLoc dl declNames
declNames DSig{}        = return mempty

-- | Names introduced by a module binding.
modBindNames :: GetNames ModBind
modBindNames ModBind { .. } =
  addLoc mbName $ \ ns -> pushNamespace ns $
    do n'    <- newName mbName
       names <- modExprNames mbExpr
       return (envMod ns [n'] `mappend` qualify (pnameToPrefix ns) names)

-- | The names introduced by a data declaration.
dataNames :: GetNames Data
dataNames Data { .. } =
  do tyName   <- newName dName
     conNames <- traverse (`addLoc` constrName) dConstrs
     return $ mconcat
            $ envType (thing dName) [tyName] : conNames

-- | Names introduced by a constructor.
constrName :: GetNames Constr
constrName Constr { .. } =
  do conName <- newName cName
     return (envDecl (thing cName) [conName])

-- | Names defined by a module expression.
modExprNames :: GetNames ModExpr
modExprNames (MEStruct ms)       = modStructNames ms
modExprNames (MEConstraint me _) = modExprNames me
modExprNames (MELoc ml)          = addLoc ml modExprNames
modExprNames _                   = return mempty

-- | Names defined by a module structure.
modStructNames :: GetNames ModStruct
modStructNames ModStruct { .. } =
  mergeNames overlapErrors declNames msElems

-- | Names defined by a pattern.
patNames :: GetNames Pat
patNames (PCon _ ps) = mergeNames overlapErrors patNames ps
patNames (PLoc lp)   = addLoc lp patNames
patNames PWild       = return mempty
patNames (PVar ln)   =
  do n <- newName ln
     return (envDecl (thing ln) [n])


-- Renaming --------------------------------------------------------------------

type Rename f = f PName -> RN (f Name)

rnLoc :: (a -> RN b) -> Located a -> RN (Located b)
rnLoc f Located { .. } = withLoc locRange $
  do b <- f locValue
     return Located { locValue = b, .. }


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
rnModStruct (ModStruct ds) = ModStruct <$> traverse rnDecl ds

-- | Rename a declaration.
rnDecl :: Rename Decl
rnDecl (DBind b)     = DBind    <$> rnBind b
rnDecl (DModBind mb) = DModBind <$> rnModBind mb
rnDecl (DLoc d)      = DLoc     <$> rnLoc rnDecl d
rnDecl (DSig s)      = panic "rename" $ text "Unexpected signature found"
                                     $$ text (show s)

-- | Rename a module binding.
rnModBind :: Rename ModBind
rnModBind ModBind { .. } = pushNamespace (thing mbName) $
  do n' <- rnLoc rnMName mbName
     e' <- underMod mbName (rnModExpr mbExpr)
     return ModBind { mbName = n', mbExpr = e' }

rnModExpr :: Rename ModExpr
rnModExpr (MEName n)           = MEName   <$> rnMName n
rnModExpr (MEApp f x)          = MEApp    <$> rnModExpr f <*> rnModExpr x
rnModExpr (MEStruct s)         = MEStruct <$> rnModStruct s
rnModExpr (MEFunctor a sig e)  = undefined
rnModExpr (MEConstraint n sig) = undefined
rnModExpr (MELoc lm)           = MELoc <$> rnLoc rnModExpr lm


-- Expressions -----------------------------------------------------------------

-- | Rename a binding. This assumes that new names have already been introduced
-- externally.
rnBind :: Rename Bind
rnBind Bind { .. } =
  do n'  <- rnLoc rnEName bName
     mb' <- traverse rnSchema bSchema
     b'  <- rnMatch bBody
     return Bind { bName = n', bSchema = mb', bBody = b' }

rnMatch :: Rename Match
rnMatch (MSplit l r) = MSplit <$> rnMatch l <*> rnMatch r
rnMatch MFail        = return MFail
rnMatch (MExpr e)    = MExpr <$> rnExpr e
rnMatch (MLoc lm)    = MLoc  <$> rnLoc rnMatch lm
rnMatch (MPat p m)   =
  do names <- patNames p
     withNames names (MPat <$> rnPat p <*> rnMatch m)

rnPat :: Rename Pat
rnPat  = undefined

-- | Rename an expression.
rnExpr :: Rename Expr
rnExpr (EVar pn)   = EVar <$> rnEName pn
rnExpr (ECon pn)   = ECon <$> rnEName pn
rnExpr (EApp f xs) = EApp <$> rnExpr f <*> traverse rnExpr xs
rnExpr (EAbs m)    = EAbs <$> rnMatch m
rnExpr (ELoc e)    = ELoc <$> rnLoc rnExpr e
rnExpr (ELit l)    = pure (ELit l)
rnExpr (ELet ds e) =
  do names <- mergeNames overlapErrors letDeclNames ds
     withNames names (ELet <$> traverse rnLetDecl ds <*> rnExpr e)



rnLetDecl :: Rename LetDecl
rnLetDecl (LDBind b) = LDBind <$> rnBind b
rnLetDecl (LDLoc ld) = LDLoc  <$> rnLoc rnLetDecl ld
rnLetDecl LDSig{}    = panic "renamer" (text "signature found in let binding")


-- Types -----------------------------------------------------------------------

rnSchema :: Rename Schema
rnSchema  = error "rnSchema"


-- Errors/Warnings -------------------------------------------------------------

conflict :: PName -> [Name] -> RN Name
conflict d ns =
  do addError ErrRnOverlap (vcat (msg : map ppr ns))
     return (head ns)
  where
  msg = pp d <+> text "is defined in multiple places:"

shadows :: PName -> ([Name],[Name]) -> RN ()
shadows d (new,old)
  | null new || null old = panic "renamer" (text "Invalid use of `shadows`")
  | otherwise            = addWarning WarnRnShadowing msg
  where
  msg = text "the definition of"
    <+> pp (head new)
    <+> text "shadows the definition of"
    <+> pp (head old)

-- | Invent a name for a parsed name, and record an error about a missing
-- identifier.
unknown :: Doc -> PName -> RN Name
unknown ty d =
  do addError ErrRnUnknown (ty <+> text "not in scope:" <+> pp d)
     loc <- askLoc
     inBase (withSupply (mkUnknown d loc))
