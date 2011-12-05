-----------------------------------------------------------------------------
--
-- Module      :  Code.Module
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

module Code.Generating.Module (
    ModuleSource,

    -- * module bodies
    ModuleBody(emptyModuleBody, mergeModuleBodys),
    SortedModuleBody(), addSModuleBody, singleSModuleBody,
    SequentialModuleBody(), addSeqModuleBody,

    -- * BuildingModule
    BuildingModule(bmoduleName),
    emptyBuildingModule,
    toModule,

    ModuleBuildPart(..),
    addBuildPart,
    addBuildParts,addBuildParts',

    -- * in case it's needed
    toImportDecls,
    toDecls

) where

import Data.List
import Data.Map hiding (map, union, filter)
import qualified Data.Map as M

import Language.Haskell.Exts.Syntax

import Code.Generating.Import
import Code.Generating.Utils

-----------------------------------------------------------------------------

type ModuleSource = String

-----------------------------------------------------------------------------

class ModuleBody m where
    toDecls :: m -> [Decl]
    emptyModuleBody :: m
    mergeModuleBodys :: m -> m -> m

-----------------------------------------------------------------------------

newtype SortedModuleBody s = SMB {smbToMap :: Map s [Decl]}
    deriving(Eq, Show)

-- WARNING DON't Use duplicate sortkeys, TODO fix duplicate keys,
-- a good key might be the name of the function that is declared
addSModuleBody :: Ord s => s -> [Decl] -> SortedModuleBody s -> SortedModuleBody s
addSModuleBody sortKey decls = SMB . M.insert sortKey decls . smbToMap

instance Ord s => ModuleBody (SortedModuleBody s) where
    toDecls = concatMap snd . sort . toList . smbToMap
    emptyModuleBody = SMB M.empty
    mergeModuleBodys baseSMB (SMB addSMB) =
        foldr (uncurry addSModuleBody) baseSMB (toList addSMB)

singleSModuleBody :: Ord s => s -> [Decl] -> SortedModuleBody s
singleSModuleBody sortKey decls = addSModuleBody sortKey decls emptyModuleBody

-----------------------------------------------------------------------------

newtype SequentialModuleBody = SeqMB {seqToList :: [Decl]}
    deriving(Eq, Show)

addSeqModuleBody :: [Decl] -> SequentialModuleBody -> SequentialModuleBody
addSeqModuleBody decls = SeqMB . (decls ++) . seqToList

instance ModuleBody SequentialModuleBody where
    toDecls = seqToList
    emptyModuleBody = SeqMB []
    mergeModuleBodys (SeqMB sq1) (SeqMB sq2) = SeqMB $ sq1 ++ sq2

-----------------------------------------------------------------------------

data BuildingModule b
    = BM
    { bmoduleName    :: ModuleName   -- ^ the name
    , bmoduleExports :: [ExportSpec] -- ^ the exported functions/modules, etc
    , bmoduleImports :: ImportList   -- ^ the list of imported functions
    , bmoduleBody    :: b            -- ^ the body
    , bmodulePostP   :: [(Module -> Module)] -- ^ post processing functions, without order
    }

emptyBuildingModule :: ModuleBody b => ModuleName -> BuildingModule b
emptyBuildingModule modname = BM modname [] emptyImportList emptyModuleBody []

toModule :: ModuleBody b => BuildingModule b -> Module
toModule (BM name exps imp body postp) = postProcess createModule
    where createModule = Module noSrcLoc name [] Nothing (Just exps)
            (toImportDecls imp) (toDecls body)
          postProcess :: Module -> Module
          postProcess = foldr (.) id postp

data ModuleBuildPart b
    = AddExport ExportSpec
    | AddImport ImportList
    | AddBody b
    | AddPostProcessor (Module -> Module)

addBuildPart :: ModuleBody b => ModuleBuildPart b -> BuildingModule b -> BuildingModule b
addBuildPart (AddExport exps) bmd = bmd{bmoduleExports = exps : bmoduleExports bmd}
addBuildPart (AddImport impl) bmd = bmd{bmoduleImports = mergeImportLists impl $ bmoduleImports bmd}
addBuildPart (AddBody   body) bmd = bmd{bmoduleBody = mergeModuleBodys body $ bmoduleBody bmd}
addBuildPart (AddPostProcessor pp) bmd = bmd{bmodulePostP = pp:bmodulePostP bmd}

addBuildParts :: ModuleBody b => [ModuleBuildPart b] -> BuildingModule b -> BuildingModule b
addBuildParts bps bm = foldr addBuildPart bm bps

addBuildParts':: ModuleBody b => BuildingModule b -> [ModuleBuildPart b] -> BuildingModule b
addBuildParts' = flip addBuildParts
