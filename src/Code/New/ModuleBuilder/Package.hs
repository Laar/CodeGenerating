-----------------------------------------------------------------------------
--
-- Module      :  Code.New.ModuleBuilder.Package
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

module Code.New.ModuleBuilder.Package (
    PackageBuild(package),
    PackageBuilder,
    singlePackage,
    liftPadjust,
    liftPquery,
    liftBuilder,
    activateModule,

    addModuleAndActivate,
    defineModule
) where

import Control.Monad.State
import Language.Haskell.Exts.Syntax

import Code.New.Package
import Code.New.ModuleBuilder.Class

data PackageBuild m
    = PackageBuild
    { package   :: Package m
    , activeMod :: ModuleName
    }

singlePackage :: (Modulelike m, BuildableModule m) => m -> PackageBuild m
singlePackage bm = PackageBuild (addExternalModule bm emptyPackage) (getModuleName bm)

activeModule :: BuildableModule m => PackageBuild m -> Maybe m
activeModule pb = getModule (activeMod pb) (package pb)

activeModule' :: BuildableModule m => PackageBuild m -> m
activeModule' pb = case activeModule pb of
    Just x -> x
    Nothing -> error $ "the active module was not found"

adjustPackage :: (Package m -> Package m) -> PackageBuild m -> PackageBuild m
adjustPackage f pb = pb{package = f $ package pb}

changeActive :: BuildableModule m => (m -> m) -> PackageBuild m -> PackageBuild m
changeActive f p = adjustPackage (changeModule (activeMod p) f) p

queryActive :: BuildableModule m => (a -> m -> b) -> a -> PackageBuild m -> b
queryActive f v p = f v (activeModule' p)

instance BuildableModule m => BuildableModule (PackageBuild m) where
    addBExport e = changeActive (addBExport e)
    addBDecls  d = changeActive (addBDecls  d)
    addBImport i = changeActive (addBImport i)
    addBPragma i = changeActive (addBPragma i)
    getBImport m = queryActive   getBImport m
    hasBPragma p = queryActive   hasBPragma p

type PackageBuilder bm m a = ModuleBuilder (PackageBuild bm) m a

liftPadjust :: (BuildableModule bm, Monad m)
    => (Package bm -> Package bm) -> PackageBuilder bm m ()
liftPadjust f = liftAdjust (adjustPackage f)

liftPquery :: (BuildableModule bm, Monad m)
    => (a -> Package bm -> b) -> a -> PackageBuilder bm m b
liftPquery f = liftQuery (\v -> f v . package)

get' :: (BuildableModule bm, Monad m) => (PackageBuild bm -> a) -> PackageBuilder bm m a
get' f = liftState $ get >>= return . f

getActiveModule :: (BuildableModule bm, Monad m)
    => PackageBuilder bm m bm
getActiveModule = get' activeModule'

getActiveMod :: (BuildableModule bm, Monad m)
    => PackageBuilder bm m ModuleName
getActiveMod = get' activeMod

putActiveModule :: (BuildableModule bm, Monad m)
    => bm -> PackageBuilder bm m ()
putActiveModule bm = do
    amn <- getActiveMod
    liftPadjust $ changeModule amn (\_ -> bm)

liftBuilder :: (BuildableModule bm, Monad m)
    => ModuleBuilder bm m a -> PackageBuilder bm m a
liftBuilder mb = do
    am <- getActiveModule
    (r, am') <- lift  $ runBuilder am mb
    putActiveModule am'
    return r

activateModule :: (BuildableModule bm, Monad m)
    => ModuleName -> PackageBuilder bm m ()
activateModule mn = do
    hm <- liftPquery hasModule mn
    if not hm
     then error $ "Module " ++ show mn ++ " is not in the package"
     else liftAdjust $ \pb -> pb{activeMod = mn}

addModuleAndActivate :: (BuildableModule bm, Modulelike bm, Monad m)
     => ModuleName -> Bool -> PackageBuilder bm m ()
addModuleAndActivate mn ext = do
    hasMod <- liftPquery hasModule mn
    when (not hasMod) $ do
        liftPadjust $ if ext then addExternalModule' mn else addInternalModule' mn
    activateModule mn

defineModule :: (BuildableModule bm, Modulelike bm, Monad m)
    => ModuleName -> Bool -> ModuleBuilder bm m a -> PackageBuilder bm m a
defineModule mn loc bm = do
    addModuleAndActivate mn loc
    liftBuilder bm
