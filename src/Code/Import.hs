-----------------------------------------------------------------------------
--
-- Module      :  Code.Import
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

module Code.Import (
    -- * module imports
    Import(..),

    ImportList,
    emptyImportList, addImport, addImports,
    mergeImportLists,


    singleFImport, singleMImport,

    -- * internals for code generation
    toImportDecls,

    defaultImport,
    defaultImport',

    hidingImport,
    partialImport,
    importName
) where

import Data.Foldable(foldr')
import Data.List
import Data.Map hiding (map, union, filter)
import qualified Data.Map as M

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

data Import
    = ImportFull
    | ImportPartial [String]
    | ImportHiding  [String]
    deriving (Eq, Ord, Show)

toImportDecl :: ModuleName -> Import -> ImportDecl
toImportDecl name ImportFull            = defaultImport name
toImportDecl name (ImportPartial funcs) = partialImport name funcs
toImportDecl name (ImportHiding  funcs) = hidingImport  name funcs

mergeImport :: Import -> Import -> Maybe Import
mergeImport ImportFull          _                  = Just ImportFull
mergeImport _                   ImportFull         = Just ImportFull
mergeImport (ImportHiding f1)   (ImportHiding f2)  = Just $ ImportHiding (filter (\e -> not $ elem e f1) f2)
mergeImport (ImportPartial f1)  (ImportPartial f2) = Just $ ImportPartial $
    foldr' (\x -> if x `elem` f1 then id else (:)x ) f1 f2
mergeImport _                   _                  = Nothing

mergeImports :: Import -> [Import] -> [Import]
mergeImports imp = go
    where go []       = [imp]
          go (im:ims) = case mergeImport imp im of
                            Just nim -> nim:ims
                            Nothing -> im : go ims

-----------------------------------------------------------------------------

type ImportList = M.Map ModuleName [Import]

emptyImportList :: ImportList
emptyImportList = M.empty
--
addImport :: ModuleName -> Import -> ImportList -> ImportList
addImport modname value impList = case M.lookup modname impList of
    Nothing -> M.insert modname [value] impList
    Just im -> M.insert modname (mergeImports value im) impList

--
addImports :: ModuleName -> [Import] -> ImportList -> ImportList
addImports modname values impList =
    let foldMerge = foldr mergeImports
    in case M.lookup modname impList of
    Nothing -> M.insert modname (foldMerge [] values) impList
    Just mo -> M.insert modname (foldMerge mo values) impList

toImportDecls :: ImportList -> [ImportDecl]
toImportDecls = concatMap toImportDecls' . sort . toList
    where toImportDecls' :: (ModuleName, [Import]) -> [ImportDecl]
          toImportDecls' (modname, imps) = map (toImportDecl modname) imps

mergeImportLists :: ImportList -> ImportList -> ImportList
mergeImportLists baseIMPL addIMPL =
        foldr (uncurry addImports) baseIMPL (toList addIMPL)

singleFImport :: ModuleName -> [String] -> ImportList
singleFImport mname funcs = M.singleton mname [ImportPartial funcs]

singleMImport :: ModuleName -> ImportList
singleMImport mname = M.singleton mname [ImportFull]

-----------------------------------------------------------------------------

defaultImport :: ModuleName -> ImportDecl
defaultImport name
    = ImportDecl {
    importLoc         = SrcLoc "" 0 0,
    importModule      = name,
    importQualified   = False,
    importSrc         = False,
    importPkg         = Nothing,
    importAs          = Nothing,
    importSpecs       = Nothing}

defaultImport' :: String -> ImportDecl
defaultImport' = defaultImport . ModuleName

-- | make a ImportDecl for importing while hiding listed functions of the module
-- (DON'T USE with classes, datatypes or type synonyms. )
hidingImport :: ModuleName -> [String] -> ImportDecl
hidingImport name hide = defImp{importSpecs = impSpecs}
    where defImp = defaultImport name
          impSpecs = Just (True, map importName $ nub hide)

-- | make a ImportDecl for importing only the listed functions of the module
-- (DON'T USE with classes, datatypes or type synonyms. )
partialImport :: ModuleName -> [String] -> ImportDecl
partialImport name part = defImp{importSpecs = impSpecs}
    where defImp = defaultImport name
          impSpecs = Just (False, map importName $ nub part)

importName :: String -> ImportSpec
importName = IVar . Ident
