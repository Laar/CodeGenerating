-----------------------------------------------------------------------------
--
-- Module      :  Code.New.Package
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

module Code.New.Package (
    Package(), emptyPackage,

    addInternalModule, addInternalModule',
    addExternalModule, addExternalModule',

    changeModule,

    hasModule,
    listModules,

    writeModules,
) where

import qualified Data.Map as M
import System.FilePath
import Language.Haskell.Exts.Pretty(prettyPrint)
import Language.Haskell.Exts.Syntax
import Code.Utils

data Package
    = Package
    { internalMods :: M.Map ModuleName Module
    , externalMods :: M.Map ModuleName Module
    }

emptyPackage :: Package
emptyPackage = Package M.empty M.empty



addInternalModule :: Module -> Package -> Package
addInternalModule m p = p{internalMods = M.insert name m $ internalMods p}
    where name = moduleToModuleName m

addInternalModule' :: ModuleName -> Package -> Package
addInternalModule' n = addInternalModule (emptyModule n)

addExternalModule :: Module -> Package -> Package
addExternalModule m p = p{externalMods = M.insert name m $ externalMods p}
    where name = moduleToModuleName m

addExternalModule' :: ModuleName -> Package -> Package
addExternalModule' n = addExternalModule (emptyModule n)

changeModule :: ModuleName -> (Module -> Module) -> Package -> Package
changeModule mn f p = p{internalMods = ints', externalMods = exts'}
    where
        ints' = M.adjust f mn $ internalMods p
        exts' = M.adjust f mn $ externalMods p

-- | query for the existance of a module
hasModule :: ModuleName -> Package -> Bool
hasModule m p = (M.member m $ internalMods p) || (M.member m $ externalMods p)

-- | List the modules, first the external ones, secondly the internal.
listModules :: Package -> ([ModuleName], [ModuleName])
listModules p = (M.keys $ externalMods p, M.keys $ internalMods p)

processModules :: (ModuleName -> Module -> IO(), ModuleName -> Module -> IO()) -> Package -> IO ()
processModules (f, g) p = sequence_ $ map f' exts ++ map g' ints
    where
        f' = uncurry f
        g' = uncurry g
        ints = M.toList $ internalMods p
        exts = M.toList $ externalMods p

processModules' :: (ModuleName -> Module -> IO()) -> Package -> IO ()
processModules' f = processModules (f,f)

-- | Write the modules to a specific directory
writeModules :: FilePath -> Package -> IO ()
writeModules fp = processModules' writeModule
    where
        writeModule mn m = writeFile (fp </> moduleNameToPath mn) (prettyPrint m)
