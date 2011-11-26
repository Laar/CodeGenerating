-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.Unsorted
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Code.Utils.Unsorted (
    otherwiseRhs,

    enumConstructor,
    derive,
) where

import Language.Haskell.Exts.Syntax

import Code.Utils.No
import Code.Utils.Syntax.Exp
import Code.Utils.Syntax.Names

otherwiseRhs :: Exp -> GuardedRhs
otherwiseRhs = GuardedRhs noSrcLoc  [Qualifier otherwiseExp]


enumConstructor :: Name -> QualConDecl
enumConstructor name = QualConDecl noSrcLoc [] [] $ ConDecl name []

derive :: String -> Deriving
derive n = (unQual n, [])
