-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Pat
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

module Code.Generating.Utils.Syntax.Pat (
    pVar',
) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

pVar' :: String -> Pat
pVar' = PVar . Ident

-----------------------------------------------------------------------------
