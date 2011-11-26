-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.Syntax.Pat
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

module Code.Utils.Syntax.Pat (
    patVar,
) where

import Language.Haskell.Exts.Syntax

patVar :: String -> Pat
patVar = PVar . Ident
