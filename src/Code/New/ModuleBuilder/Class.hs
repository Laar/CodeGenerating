{-# LANGUAGE
    GeneralizedNewtypeDeriving, StandaloneDeriving,
    FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances
 #-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.New.ModuleBuilder.Class
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

module Code.New.ModuleBuilder.Class (
    BuildableModule(..),
    ModuleBuilder(),
    runBuilder, execBuilder,

    addExport,
    addDecls, addDecl,
    addImport,
    addPragma,
    getImport,
    hasPragma,
) where

import Control.Monad.Reader.Class
import Control.Monad.State

import Language.Haskell.Exts.Syntax

class BuildableModule b where
    emptyBModule :: ModuleName        -> b
    addBExport   :: ExportSpec   -> b -> b
    addBDecls    :: [Decl]       -> b -> b
    addBImport   :: ImportDecl   -> b -> b
    addBPragma   :: ModulePragma -> b -> b
    getBImport   :: ModuleName   -> b -> Maybe ImportDecl
    hasBPragma   :: ModulePragma -> b -> Bool

runBuilder :: (BuildableModule bm, Monad m) => bm -> ModuleBuilder bm m a -> m (a, bm)
runBuilder sm builder = runStateT (getBState builder) sm

execBuilder :: (BuildableModule bm, Monad m) => bm -> ModuleBuilder bm m a -> m bm
execBuilder sm builder = runBuilder sm builder >>= return . snd

newtype ModuleBuilder s m a = ModuleBuilder {getBState :: StateT s m a}

deriving instance Monad m => Monad (ModuleBuilder s m)
deriving instance (MonadReader r m) => MonadReader r (ModuleBuilder s m)

liftModify :: (BuildableModule bm, Monad m) => (a -> bm -> bm) -> a -> ModuleBuilder bm m ()
liftModify f v = ModuleBuilder $ modify (f v)

addExport :: (BuildableModule bm, Monad m) => ExportSpec -> ModuleBuilder bm m ()
addExport = liftModify addBExport

addDecls :: (BuildableModule bm, Monad m) => [Decl] -> ModuleBuilder bm m ()
addDecls = liftModify addBDecls

addDecl ::  (BuildableModule bm, Monad m) => Decl -> ModuleBuilder bm m ()
addDecl d = addDecls [d]

addImport :: (BuildableModule bm, Monad m) => ImportDecl   -> ModuleBuilder bm m ()
addImport = liftModify addBImport

addPragma :: (BuildableModule bm, Monad m) => ModulePragma -> ModuleBuilder bm m ()
addPragma = liftModify addBPragma

liftQuery :: (BuildableModule bm, Monad m) => (a -> bm -> b) -> a -> ModuleBuilder bm m b
liftQuery f v = ModuleBuilder $ get >>= \m -> return $ f v m

getImport :: (BuildableModule bm, Monad m) => ModuleName -> ModuleBuilder bm m (Maybe ImportDecl)
getImport = liftQuery getBImport

hasPragma :: (BuildableModule bm, Monad m) => ModulePragma -> ModuleBuilder bm m Bool
hasPragma = liftQuery hasBPragma

