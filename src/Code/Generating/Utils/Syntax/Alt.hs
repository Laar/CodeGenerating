-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.Syntax.Alt
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

module Code.Generating.Utils.Syntax.Alt (
    toAlt, toAlt',
) where

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.No

toAlt :: Pat -> Exp -> Alt
toAlt p e = Alt noSrcLoc p (UnGuardedAlt e) (BDecls [])

toAlt' :: (Pat, Exp) -> Alt
toAlt' = uncurry toAlt


