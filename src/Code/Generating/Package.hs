-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Package
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Several modules are grouped in a package, this module provides such a
-- grouping in the form of `Package`, It allows to add and change modules
-- and other modifications.
--
-----------------------------------------------------------------------------

module Code.Generating.Package (
    Package(), Modulelike(..),
    emptyPackage,

    addInternalModule, addInternalModule',
    addExternalModule, addExternalModule',

    changeModule,

    hasModule, getModule,
    listModules,

    processModules,
    processModules',
    writeModules,

    safeWriteFile,
) where

-----------------------------------------------------------------------------

import qualified Data.Map as M
import System.Directory
import System.FilePath

import Language.Haskell.Exts.Pretty(prettyPrint)
import Language.Haskell.Exts.Syntax
import Code.Generating.Utils


-----------------------------------------------------------------------------

data Package m
    = Package
    { internalMods :: M.Map ModuleName m
    , externalMods :: M.Map ModuleName m
    }

class Modulelike m where
    emptyMod      :: ModuleName -> m
    getModuleName :: m -> ModuleName

instance Modulelike Module where
    emptyMod      = emptyModule
    getModuleName = moduleToModuleName

emptyPackage :: Modulelike p => Package p
emptyPackage = Package M.empty M.empty

-----------------------------------------------------------------------------

addInternalModule :: Modulelike m => m -> Package m -> Package m
addInternalModule m p = p{internalMods = M.insert name m $ internalMods p}
    where name = getModuleName m

addInternalModule' :: Modulelike m => ModuleName -> Package m -> Package m
addInternalModule' n = addInternalModule (emptyMod n)

addExternalModule :: Modulelike m => m -> Package m -> Package m
addExternalModule m p = p{externalMods = M.insert name m $ externalMods p}
    where name = getModuleName m

addExternalModule' :: Modulelike m => ModuleName -> Package m -> Package m
addExternalModule' n = addExternalModule (emptyMod n)

changeModule :: ModuleName -> (m -> m) -> Package m -> Package m
changeModule mn f p = p{internalMods = ints', externalMods = exts'}
    where
        ints' = M.adjust f mn $ internalMods p
        exts' = M.adjust f mn $ externalMods p

-- | query for the existance of a module
hasModule :: ModuleName -> Package m -> Bool
hasModule m p = (M.member m $ internalMods p) || (M.member m $ externalMods p)

getModule :: ModuleName -> Package m -> Maybe m
getModule mn p = case M.lookup mn $ internalMods p of
    Just x -> Just x
    Nothing -> M.lookup mn $ externalMods p

-- | List the modules, first the external ones, secondly the internal.
listModules :: Package m -> ([ModuleName], [ModuleName])
listModules p = (M.keys $ externalMods p, M.keys $ internalMods p)

processModules :: Monad m
    => (ModuleName -> Module -> m (), ModuleName -> Module -> m ())
    -> Package Module -> m ()
processModules (f, g) p = sequence_ $ map f' exts ++ map g' ints
    where
        f' = uncurry f
        g' = uncurry g
        ints = M.toList $ internalMods p
        exts = M.toList $ externalMods p

processModules' :: Monad m => (ModuleName -> Module -> m ())
    -> Package Module -> m ()
processModules' f = processModules (f,f)

-- | Write the modules to a specific directory
writeModules :: FilePath -> Package Module -> IO ()
writeModules fp = processModules' writeModule
    where
        writeModule mn m = safeWriteFile (fp </> moduleNameToPath mn <.> "hs") (prettyPrint m)

-----------------------------------------------------------------------------

-- | `safeWriteFile` is an extended version of writefile that also creates
-- the directory to a file if it does not yet exist.
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile fp' fc = createDirectoryIfMissing True (dropFileName fp')
    >> writeFile fp' fc

-----------------------------------------------------------------------------
