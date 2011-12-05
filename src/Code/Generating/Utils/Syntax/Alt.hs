-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Alt
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Code.Generating.Utils.Syntax.Alt (
    toAlt, toAlt',
) where

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.No

toAlt :: Pat -> Exp -> Alt
toAlt p e = Alt noSrcLoc p (UnGuardedAlt e) (BDecls [])

toAlt' :: (Pat, Exp) -> Alt
toAlt' = uncurry toAlt


