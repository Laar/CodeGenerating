-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Builder.Package
-- Copyright   :  (c) 2011-2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Generalizing `BuildableModule` to `Package` to build packages of
-- `BuildableModule`s.
--
-----------------------------------------------------------------------------

module Code.Generating.Builder.Package (
    PackageBuild(packages),
    PackageBuilder,
    singlePackage,
    liftPadjust,
    liftPquery,
    addModule,
    liftModBuilder,

    defineModule
) where

-----------------------------------------------------------------------------

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Language.Haskell.Exts.Syntax

import Code.Generating.Package
import Code.Generating.Builder.ModuleBuilder

-----------------------------------------------------------------------------

data PackageBuild m
    = PackageBuild
    { packages  :: Package m
    }

type PackageBuilder bm m = StateT (PackageBuild bm) m

-----------------------------------------------------------------------------

singlePackage :: (Modulelike m, BuildableModule m) => m -> PackageBuild m
singlePackage bm = PackageBuild (addExternalModule bm emptyPackage)

adjustPackage :: (Package m -> Package m) -> PackageBuild m -> PackageBuild m
adjustPackage f pb = pb{packages = f $ packages pb}

-----------------------------------------------------------------------------

liftPadjust :: Monad m
    => (Package bm -> Package bm) -> PackageBuilder bm m ()
liftPadjust f = modify (adjustPackage f)

liftPquery :: (Monad m)
    => (a -> Package bm -> b) -> a -> PackageBuilder bm m b
liftPquery f v = gets (f v . packages)

-----------------------------------------------------------------------------

liftModBuilder :: (BuildableModule bm, Monad m)
    => ModuleName -> SModuleBuilder bm m a -> PackageBuilder bm m (Maybe a)
liftModBuilder mn mb = do
    mMod <- liftPquery getModule mn
    case mMod of
        Nothing -> return Nothing
        Just mo -> do
                (r, mo') <- lift $ runStateT mb mo
                liftPadjust (changeModule mn (const mo'))
                return $ Just r

--liftAddModBuilder :: (BuildableModule bm, Monad m)
--    => ModuleName -> ModuleBuilder bm m a -> PackageBuilder bm m a

-----------------------------------------------------------------------------


addModule :: (BuildableModule bm, Modulelike bm, Monad m)
     => ModuleName -> Bool -> PackageBuilder bm m ()
addModule mn ext = do
    hasMod <- liftPquery hasModule mn
    when (not hasMod) $ do
        liftPadjust $ if ext then addExternalModule' mn else addInternalModule' mn

defineModule :: (BuildableModule bm, Modulelike bm, Monad m)
    => ModuleName -> Bool -> SModuleBuilder bm m a -> PackageBuilder bm m a
defineModule mn loc bm = do
    addModule mn loc
    liftModBuilder mn bm >>= return . fromMaybe (error $ "Module not added")

-----------------------------------------------------------------------------
