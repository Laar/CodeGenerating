-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Type
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

module Code.Generating.Utils.Syntax.Type (
    tCon, tyCon,
    tyVar, addIOType,
    (-->>),
    unBangType,
) where

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.Syntax.Names

infixr 5 -->>

tyVar :: String -> Type
tyVar = TyVar . Ident

tyCon :: String -> Type
tyCon = TyCon . unQual

tCon :: Name -> Type
tCon = TyCon . UnQual

addIOType :: Type -> Type
addIOType = TyApp (tyCon "IO")

-- | Alias for (->) types
(-->>) :: Type -> Type -> Type
(-->>) = TyFun

unBangType :: BangType -> Type
unBangType (UnBangedTy t) = t
unBangType (BangedTy   t) = t
unBangType (UnpackedTy t) = t
