
module Dang.Core.Interface where

import Dang.Core.AST ( Module )
import Dang.ModuleSystem.Interface ( Iface )
import Dang.Utils.Panic ( panic )
import Dang.Utils.Pretty

moduleIface :: Module -> Iface
moduleIface  = panic "Dang.Core.Interface" (text "not implemented")
