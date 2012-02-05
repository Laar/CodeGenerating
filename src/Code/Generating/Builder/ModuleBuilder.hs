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
    SinglePragma(..),
    modulePragmaToSingles, singleToModulePragma, pragmaExtends,

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
import Code.Generating.Utils

-----------------------------------------------------------------------------

data SinglePragma
    = LanguageP Name
    | OptionsP (Maybe Tool) String
    | AnnotationP Annotation
    deriving(Eq, Ord, Show)

singleToModulePragma :: SinglePragma -> ModulePragma
singleToModulePragma sp = case sp of
    LanguageP   n       -> LanguagePragma   noSrcLoc [n]
    OptionsP    mt s    -> OptionsPragma    noSrcLoc mt s
    AnnotationP a       -> AnnModulePragma  noSrcLoc a

modulePragmaToSingles :: ModulePragma -> [SinglePragma]
modulePragmaToSingles mp = case mp of
    LanguagePragma  _ ps    -> map LanguageP ps
    OptionsPragma   _ mt s  -> [OptionsP mt s]
    AnnModulePragma _ a     -> [AnnotationP a]

pragmaExtends :: SinglePragma -> SinglePragma -> Bool
pragmaExtends p1 p2 | p1 == p2 = True
pragmaExtends (OptionsP (Just _) s1) (OptionsP Nothing s2) = s1 == s2
pragmaExtends _  _ = False

-----------------------------------------------------------------------------

class BuildableModule b where
    addBExport   :: ExportSpec   -> b -> b
    addBDecls    :: [Decl]       -> b -> b
    addBImport   :: ImportDecl   -> b -> b
    addBPragma   :: SinglePragma -> b -> b
    getBImport   :: ModuleName   -> b -> [ImportDecl]
    hasBPragma   :: SinglePragma -> b -> Bool

-----------------------------------------------------------------------------

class Monad m => ModuleBuilder m where
    addExport :: ExportSpec     -> m ()
    addDecls  :: [Decl]         -> m ()
    addImport :: ImportDecl     -> m ()
    addPragma :: SinglePragma   -> m ()
    getImport :: ModuleName     -> m [ImportDecl]
    hasPragma :: SinglePragma   -> m Bool

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
