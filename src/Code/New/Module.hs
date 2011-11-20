--{-# OPTIONS_GHC -fno-warn-unused-binds#-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.New.Module
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

module Code.New.Module (
    Module',
    mkModule, toModule,

    addDecls,
) where

import Language.Haskell.Exts.Syntax
import Code.Utils

import Code.New.Symbol
import Code.New.SymbolTable

-----------------------------------------------------------------------------

data Module'
    = Module'
    { modName   :: ModuleName
--    , modDSymbols  :: M.Map Name  FDSym
--    , modTSymbols :: M.Map TName TSym
    , modSyms   :: SymbolTable
    , modDecls  :: [Decl]
    }

toModule :: Module' -> Module
toModule m
    = Module noSrcLoc (modName m) [] Nothing (Just exps) imps decls
    where
        exps  = mkExportSpec $ modSyms m
        imps  = mkImportSpec $ modSyms m
        decls = modDecls m

mkModule :: ModuleName -> Module'
mkModule n = Module' n emptyTable []

-------------------------------------------------------------------------------

addDecls :: [Decl] -> Module' -> Module'
addDecls dcls m = m{modDecls = dcls ++ modDecls m}



-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
