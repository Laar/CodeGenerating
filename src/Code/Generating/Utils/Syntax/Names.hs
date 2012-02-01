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

import Control.Arrow(first)
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
qual' ('(':nr) = case reverse nr of
    [] -> error "qual' only a '('"
    (')':n) -> case symModSplit (reverse n) of
        ([], s) -> UnQual . Symbol $ s
        ( m, s) -> Qual (ModuleName m) (Symbol s)
    _ -> error "qual' unmatched '('"
qual' n =
    let (rname, rmodu) =  break (== '.') $ reverse n
        name = Ident $ reverse rname
    in case rmodu of
        [] -> UnQual name
        rmod ->  Qual (ModuleName . reverse $ tail rmod) name


symModSplit :: String -> (String, String)
symModSplit n =
    let modrest = span (`elem` nonSyms) n
    in case modrest of
        ([], _) -> modrest
        (m, '.':r) -> first (\m' -> m ++ (if null m' then id else ('.':) ) m')
                        $ symModSplit r
        _ -> error $ "Module part not followed by symbol in (" ++ show n ++ ")"
    where
        nonSyms = ['A'..'z'] ++ ['0'..'9'] ++ "_"
-----------------------------------------------------------------------------
