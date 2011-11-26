-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.No
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

module Code.Utils.No (
    noSrcLoc,
    noBinds,
    noContext,
) where

import Language.Haskell.Exts.Syntax

noContext :: Context
noContext = []

noSrcLoc :: SrcLoc
noSrcLoc = error $ "no source location"

noBinds :: Binds
noBinds = BDecls []
