-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Builder.ModuleBuilder
-- Copyright   :  (c) 2011-2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | `BuildableModule` class representing the actions to build a module.
-- As a module is mostlikly to be build in an enviroment some lifting
-- functions for using the `BuildableModule` functions as a `StateT` state
-- are defined.
--
-----------------------------------------------------------------------------

module Code.Generating.Builder.ModuleBuilder (
    BuildableModule(..),
    ModuleBuilder(),

    addExport,
    addDecls, addDecl,
    addImport,
    addPragma,
    getImport,
    hasPragma,

    queryBuilder,
) where

import Control.Monad.State

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

class BuildableModule b where
    addBExport   :: ExportSpec   -> b -> b
    addBDecls    :: [Decl]       -> b -> b
    addBImport   :: ImportDecl   -> b -> b
    addBPragma   :: ModulePragma -> b -> b
    getBImport   :: ModuleName   -> b -> Maybe ImportDecl
    hasBPragma   :: ModulePragma -> b -> Bool

-----------------------------------------------------------------------------

type ModuleBuilder b m = StateT b m

-----------------------------------------------------------------------------

addExport :: (BuildableModule bm, Monad m) => ExportSpec -> ModuleBuilder bm m ()
addExport = modify . addBExport

addDecls :: (BuildableModule bm, Monad m) => [Decl] -> ModuleBuilder bm m ()
addDecls = modify . addBDecls

addDecl ::  (BuildableModule bm, Monad m) => Decl -> ModuleBuilder bm m ()
addDecl d = addDecls [d]

addImport :: (BuildableModule bm, Monad m) => ImportDecl   -> ModuleBuilder bm m ()
addImport = modify . addBImport

addPragma :: (BuildableModule bm, Monad m) => ModulePragma -> ModuleBuilder bm m ()
addPragma = modify . addBPragma

queryBuilder :: (BuildableModule bm, Monad m) => (a -> bm -> b) -> a -> ModuleBuilder bm m b
queryBuilder f v = get >>= \m -> return $ f v m

getImport :: (BuildableModule bm, Monad m) => ModuleName -> ModuleBuilder bm m (Maybe ImportDecl)
getImport = queryBuilder getBImport

hasPragma :: (BuildableModule bm, Monad m) => ModulePragma -> ModuleBuilder bm m Bool
hasPragma = queryBuilder hasBPragma

-----------------------------------------------------------------------------
