module Dang.ModuleSystem.Imports (
    gatherImports
  ) where

import Dang.ModuleSystem.Interface
import Dang.ModuleSystem.QualName
import Dang.Monad
import Dang.Syntax.AST
import Dang.Utils.Location
import Dang.Utils.Pretty

import           Data.Data ( Data, gmapQl )
import           Data.Generics ( extQ )
import           Data.Monoid ( mempty )
import qualified Data.Set as Set


gatherImports :: Module -> Dang IfaceSet
gatherImports m =
  do let needed = impQ m
     logInfo (text "Needed interfaces:" $$ text (show needed))
     return mempty

data ImportOrigin = FromOpen ModName
                  | FromName ModName
                    deriving (Show,Eq,Ord)

impQ :: Data a => a -> Set.Set ImportOrigin
impQ  = gmapQl Set.union Set.empty impQ `extQ`
        impName `extQ`
        impOpen

-- | Modules named by import declarations
impOpen :: Open -> Set.Set ImportOrigin
impOpen o = Set.singleton (FromOpen (unLoc (openMod o)))

impName :: Name -> Set.Set ImportOrigin
impName (QualName _ ns a) = Set.singleton (FromName ns)
impName (LocalName _ _)   = mempty
