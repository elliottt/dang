{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Dang.Syntax.AST where


import Dang.Syntax.Location
import Dang.Utils.Ident
import Dang.Utils.PP

import qualified Data.Text.Lazy as L
import           GHC.Generics (Generic)


-- | Parsed names, either qualified or unqualified.
data PName = PUnqual !L.Text
           | PQual   !L.Text !L.Text
             deriving (Eq,Show,Ord,Generic)

-- | A parsed top-level module.
type PModule = Module PName

data Module name = Module { modName  :: Namespace
                          , modDecls :: [Located (Decl name)]
                          } deriving (Show)

newtype ModStruct name = ModStruct { msElems :: [Located (Decl name)]
                                   } deriving (Eq,Show,Functor,Generic)

data Decl name = DBind    (Bind name)
               | DSig     (Sig name)
               | DModBind (ModBind name)
               | DLoc     (Located (Decl name))
                 deriving (Eq,Show,Functor,Generic)

data Bind name = Bind { bName   :: Located name
                      , bSchema :: Maybe (Schema name)
                      , bBody   :: Match name
                      } deriving (Eq,Show,Functor,Generic)

data Sig name = Sig { sigNames  :: [Located name]
                    , sigSchema :: Schema name
                    } deriving (Eq,Show,Functor,Generic)

data ModBind name = ModBind { mbName :: Located name
                            , mbExpr :: ModExpr name
                            } deriving (Eq,Show,Functor,Generic)

data ModType name = MTSig (ModSig name)
                  | MTFunctor (Located name) (ModType name) (ModType name)
                    -- XXX add with-constraints
                  | MTLoc (Located (ModType name))
                    deriving (Eq,Show,Functor,Generic)

data ModSig name = MSSig (Sig name)
                 | MSType (Type name)
                 | MSLoc (Located (ModSig name))
                   deriving (Eq,Show,Functor,Generic)

data ModExpr name = MEName name
                  | MEApp (ModExpr name) (ModExpr name)
                  | MEStruct (ModStruct name)
                  | MEFunctor (Located name) (ModType name) (ModExpr name)
                  | MEConstraint (ModExpr name) (ModType name)
                  | MEUnpack (Expr name)
                  | MELoc (Located (ModExpr name))
                    deriving (Eq,Show,Functor,Generic)

data Match name = MPat (Pat name) (Match name)
                | MSplit (Match name) (Match name)
                | MFail
                | MExpr (Expr name)
                | MLoc (Located (Match name))
                  deriving (Eq,Show,Functor,Generic)

data Pat name = PVar name
              | PWild
              | PLoc (Located (Pat name))
                deriving (Eq,Show,Functor,Generic)

data Expr name = EVar name
               | EApp (Expr name) (Expr name)
               | EAbs (Match name)
               | ELit Literal
               | ELoc (Located (Expr name))
                 deriving (Eq,Show,Functor,Generic)

data Schema name = Schema [name] (Type name)
                   deriving (Eq,Show,Functor,Generic)

data Type name = TCon name
               | TVar name
               | TFun (Type name) (Type name)
                 deriving (Eq,Show,Functor,Generic)

data Literal = LInt Integer Int -- ^ value and base
               deriving (Eq,Show,Generic)


-- Pretty-printing -------------------------------------------------------------

instance PP PName where
  ppr (PUnqual n)  = pp n
  ppr (PQual ns n) = pp ns <> char '.' <> pp n
