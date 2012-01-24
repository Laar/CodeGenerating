-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.No
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Several \'empty\' constructors.
--
-----------------------------------------------------------------------------

module Code.Generating.Utils.No (
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
