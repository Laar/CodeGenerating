-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.Syntax.Type
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

module Code.Utils.Syntax.Type (
    tCon, tyCon,
    tyVar, addIOType,
    (-->>),
    unBangType,
) where

import Language.Haskell.Exts.Syntax

import Code.Utils.Syntax.Names

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
