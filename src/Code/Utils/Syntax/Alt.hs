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

module Code.Utils.Syntax.Alt (
    toAlt, toAlt',
) where

import Language.Haskell.Exts.Syntax

import Code.Utils.No

toAlt :: Pat -> Exp -> Alt
toAlt p e = Alt noSrcLoc p (UnGuardedAlt e) (BDecls [])

toAlt' :: (Pat, Exp) -> Alt
toAlt' = uncurry toAlt


