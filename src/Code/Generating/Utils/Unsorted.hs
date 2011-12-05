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

module Code.Generating.Utils.Unsorted (
    otherwiseRhs,

    enumConstructor,
    derive,
) where

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.No
import Code.Generating.Utils.Syntax.Exp
import Code.Generating.Utils.Syntax.Names

otherwiseRhs :: Exp -> GuardedRhs
otherwiseRhs = GuardedRhs noSrcLoc  [Qualifier otherwiseExp]


enumConstructor :: Name -> QualConDecl
enumConstructor name = QualConDecl noSrcLoc [] [] $ ConDecl name []

derive :: String -> Deriving
derive n = (unQual n, [])