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
    Package(),
    emptyPackage,
    ModuleInfo(..),
--    getPackageInfo,
    getModule,
    newModule,
    adjustModule,
) where

import Control.Arrow((***))
import qualified Data.Map as M
--import Data.Maybe
import Language.Haskell.Exts.Syntax

import Code.New.Module

type ModuleMap = M.Map ModuleName (Module', ModuleInfo)

data Package
    = Package
    { _pName :: String
    , pMods :: ModuleMap
    }

data ModuleInfo
    = ModuleInfo
    { isExported :: Bool
    }

emptyPackage :: String -> Package
emptyPackage n = Package n M.empty

newModule :: ModuleName -> Module' -> ModuleInfo -> Package -> Package
newModule mn m mi = adjustModuleMap (M.insert mn (m,mi))

--getPackageInfo :: Info i => Name -> Package -> [(ModuleName, i)]
--getPackageInfo n p = catMaybes . map (liftMaybeS . (id *** getInfo n)) $ toModuleList p
--
--liftMaybeS :: (a, Maybe b) -> Maybe (a,b)
--liftMaybeS (_, Nothing) = Nothing
--liftMaybeS (a, Just b ) = Just (a,b)

--toModuleList :: Package -> [(ModuleName, Module')]
--toModuleList = map (id *** fst). M.toList . pMods

getModule :: Package -> ModuleName -> Maybe Module'
getModule p mn = fmap fst . M.lookup mn $ pMods p

adjustModuleMap :: (ModuleMap -> ModuleMap) -> Package -> Package
adjustModuleMap f p = p{pMods = f $ pMods p}

adjustModule :: (Module' -> Module') -> ModuleName -> Package -> Package
adjustModule f mn = adjustModuleMap (M.adjust (f *** id) mn)
