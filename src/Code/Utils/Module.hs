-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.Module
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

module Code.Utils.Module (
    emptyModule,
    emptyModule',
    exportVar, exportName,

    moduleToModuleName, moduleNameToName, moduleToName,

    addModulePragmas,
    moduleNameToPath,
) where

import Data.List(nub)
import System.FilePath(pathSeparator)
import Language.Haskell.Exts.Syntax

emptyModule :: ModuleName -> Module
emptyModule name = Module undefined name [] Nothing (Just []) [] []

emptyModule' :: String -> Module
emptyModule' = emptyModule . ModuleName

moduleToModuleName :: Module -> ModuleName
moduleToModuleName (Module _ name _ _ _ _ _) = name

moduleNameToName :: ModuleName -> String
moduleNameToName (ModuleName name) = name

moduleToName :: Module -> String
moduleToName = moduleNameToName . moduleToModuleName

addModulePragmas :: [ModulePragma] -> Module -> Module
addModulePragmas prags (Module srcl name prag wt ex im decls) =
    Module srcl name (nub (prags ++ prag)) wt ex im decls

moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName n) = foldr replace [] n
    where
        replace '.' p = pathSeparator : p
        replace  c  p = c : p


exportVar :: String -> ExportSpec
exportVar = EVar . UnQual . Ident

exportName :: Name -> ExportSpec
exportName = EVar . UnQual