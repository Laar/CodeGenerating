-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Names
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

module Code.Generating.Utils.Syntax.Names (
    unQual, unQualS, qual,
    unname,
    unQName,
    unCName,
) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

unQual :: String -> QName
unQual = UnQual . Ident

unQualS :: String -> QName
unQualS = UnQual . Symbol

qual :: ModuleName -> String -> QName
qual name = Qual name . Ident

unname :: Name -> String
unname (Ident  n) = n
unname (Symbol s) = s

unQName :: QName -> Name
unQName (UnQual n) = n
unQName (Qual _ n) = n
unQName (Special _) = error $ "unQName: can't unQualify a special con"

unCName :: CName -> Name
unCName (VarName n) = n
unCName (ConName n) = n

-----------------------------------------------------------------------------
