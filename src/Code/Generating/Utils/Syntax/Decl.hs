-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Decl
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

module Code.Generating.Utils.Syntax.Decl (
    oneLineFun, oneLiner,
    oneTypeSig,
) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax
import Code.Generating.Utils.No

-----------------------------------------------------------------------------

oneLineFun :: Name -> [Pat] -> Rhs -> Binds -> Decl
oneLineFun n ps rh bd = FunBind [Match noSrcLoc n ps Nothing rh bd]

oneLiner :: Name -> [Pat] -> Exp -> Decl
oneLiner n ps e = oneLineFun n ps (UnGuardedRhs e) noBinds

oneTypeSig :: Name -> Type -> Decl
oneTypeSig n t = TypeSig noSrcLoc [n] t

-----------------------------------------------------------------------------
