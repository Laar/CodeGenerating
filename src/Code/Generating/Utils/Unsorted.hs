-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Unsorted
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | So far unsorted helper functions.
--
-----------------------------------------------------------------------------

module Code.Generating.Utils.Unsorted (
    otherwiseRhs,

    nullaryConstructor,
    derive,
) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.No
import Code.Generating.Utils.Syntax.Exp
import Code.Generating.Utils.Syntax.Names

-----------------------------------------------------------------------------

otherwiseRhs :: Exp -> GuardedRhs
otherwiseRhs = GuardedRhs noSrcLoc  [Qualifier otherwiseExp]

-- | Constructor without arguments.
nullaryConstructor :: Name -> QualConDecl
nullaryConstructor name = QualConDecl noSrcLoc [] [] $ ConDecl name []

derive :: String -> Deriving
derive n = (unQual n, [])

-----------------------------------------------------------------------------
