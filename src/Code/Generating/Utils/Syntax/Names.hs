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
    qual',
    unQual', unQualSym', qual,
    unname,
    unQName,
    unCName,
) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

unQual' :: String -> QName
unQual' = UnQual . Ident

unQualSym' :: String -> QName
unQualSym' = UnQual . Symbol

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

-- | Makes a `QName` from a string, this string can contain a fully
-- qualified name and symbols which all will be parsed correctly.
-- SpecialCon is not handled correctly
qual' :: String -> QName
qual' n =
    let rn@(c:_) = reverse n
    in case c of
        ')' ->  let (rsym, rrest) = break (== '(') rn
                    sym = Symbol $ '('  : reverse rsym
                in case rrest of
                    ['(']       -> UnQual sym
                    ('(':'.':_) -> Qual (ModuleName . reverse . tail $ tail rrest) sym
                    _           -> error $ "qual': illegal input " ++ show n
        _   ->  let (rname, rmodu) =  break (== '.') rn
                    name = Ident $ reverse rname
                in case rmodu of
                    [] -> UnQual name
                    rmod ->  Qual (ModuleName . reverse $ tail rmod) name


-----------------------------------------------------------------------------
