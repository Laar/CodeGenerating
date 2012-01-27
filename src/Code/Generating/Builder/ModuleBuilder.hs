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
    SModuleBuilder,

    addExport,
    addDecls, addDecl,
    addImport,
    addPragma,
    getImport,
    hasPragma,

--    queryBuilder,
) where

import Control.Monad.State

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

class BuildableModule b where
    addBExport   :: ExportSpec   -> b -> b
    addBDecls    :: [Decl]       -> b -> b
    addBImport   :: ImportDecl   -> b -> b
    addBPragma   :: ModulePragma -> b -> b
    getBImport   :: ModuleName   -> b -> [ImportDecl]
    hasBPragma   :: ModulePragma -> b -> Bool

-----------------------------------------------------------------------------

class Monad m => ModuleBuilder m where
    addExport :: ExportSpec     -> m ()
    addDecls  :: [Decl]         -> m ()
    addImport :: ImportDecl     -> m ()
    addPragma :: ModulePragma   -> m ()
    getImport :: ModuleName     -> m [ImportDecl]
    hasPragma :: ModulePragma   -> m Bool

addDecl :: ModuleBuilder m => Decl -> m ()
addDecl d = addDecls [d]

-----------------------------------------------------------------------------

instance (BuildableModule bm, Monad m) => ModuleBuilder (StateT bm m) where -- SModuleBuilder
    addExport = modify . addBExport
    addDecls  = modify . addBDecls
    addImport = modify . addBImport
    addPragma = modify . addBPragma
    getImport = queryBuilder getBImport
    hasPragma = queryBuilder hasBPragma

-- | Type alias for making clear what the purpose is.
type SModuleBuilder = StateT

queryBuilder :: (BuildableModule bm, Monad m) => (a -> bm -> b) -> a -> SModuleBuilder bm m b
queryBuilder f v = get >>= \m -> return $ f v m

-----------------------------------------------------------------------------
