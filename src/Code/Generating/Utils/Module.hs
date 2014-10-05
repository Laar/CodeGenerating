-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Module
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Module related helper functions
--
-----------------------------------------------------------------------------

module Code.Generating.Utils.Module (
    emptyModule,
    emptyModule',
    eVar, eVar', iVar,

    importAll, partialImport,

    moduleToModuleName, moduleNameToName, moduleToName,

    addModulePragmas,
    moduleNameToPath,
) where

-----------------------------------------------------------------------------

import Data.List(nub)
import System.FilePath(pathSeparator)
import Language.Haskell.Exts.Syntax
import Code.Generating.Utils.No

-----------------------------------------------------------------------------

emptyModule :: ModuleName -> Module
emptyModule name = Module undefined name [] Nothing (Just []) [] []

emptyModule' :: String -> Module
emptyModule' = emptyModule . ModuleName

moduleToModuleName :: Module -> ModuleName
moduleToModuleName (Module _ name _ _ _ _ _) = name

-- | Deconstructing of ModuleName
moduleNameToName :: ModuleName -> String
moduleNameToName (ModuleName name) = name

moduleToName :: Module -> String
moduleToName = moduleNameToName . moduleToModuleName

addModulePragmas :: [ModulePragma] -> Module -> Module
addModulePragmas prags (Module srcl name prag wt ex im decls) =
    Module srcl name (nub (prags ++ prag)) wt ex im decls

-- | Converts the module name to the path of it's source code.
moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName n) = foldr replace [] n
    where
        replace '.' p = pathSeparator : p
        replace  c  p = c : p


eVar' :: String -> ExportSpec
eVar' = EVar NoNamespace . UnQual . Ident

eVar :: Name -> ExportSpec
eVar = EVar NoNamespace . UnQual

iVar :: Name -> ImportSpec
iVar = IVar NoNamespace

-- | Make an import for a module importing all content, e.g.
-- > import Data.List
importAll :: ModuleName -> ImportDecl
importAll name = ImportDecl noSrcLoc name False False False Nothing Nothing Nothing

-- | Make an import for a module restricted to the given import spec, e.g.
-- > import Data.List(nub)
partialImport :: ModuleName -> [ImportSpec] -> ImportDecl
partialImport name imports =
    ImportDecl noSrcLoc name False False False Nothing Nothing (Just (False, imports))

-----------------------------------------------------------------------------
