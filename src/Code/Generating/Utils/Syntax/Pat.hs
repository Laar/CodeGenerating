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
    patVar,
) where

import Language.Haskell.Exts.Syntax

patVar :: String -> Pat
patVar = PVar . Ident
