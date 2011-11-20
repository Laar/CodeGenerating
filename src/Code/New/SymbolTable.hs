-----------------------------------------------------------------------------
--
-- Module      :  Code.New.SymbolTable
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

module Code.New.SymbolTable (
    SymbolTable,
    emptyTable,

    isDefinedD, isDefinedT,
    addFDSym, addTSym,
    getDSym, getTSym,

    mkExportSpec, mkImportSpec,
) where

import Control.Arrow ((&&&))
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Language.Haskell.Exts.Syntax
import Code.Utils

import Code.New.Symbol

-----------------------------------------------------------------------------

data SymbolTable
    = SymbolTable
    { modDSymbols  :: M.Map Name  FDSym
    , modTSymbols :: M.Map TName TSym
    }

emptyTable :: SymbolTable
emptyTable = SymbolTable M.empty M.empty

-----------------------------------------------------------------------------

alterFDSyms :: (M.Map Name FDSym -> M.Map Name FDSym) -> SymbolTable -> SymbolTable
alterFDSyms f m = m{modDSymbols = f $ modDSymbols m}

alterTSyms :: (M.Map TName TSym -> M.Map TName TSym) -> SymbolTable -> SymbolTable
alterTSyms f m = m{modTSymbols = f $ modTSymbols m}

-----------------------------------------------------------------------------

isDefinedD :: SymbolTable -> Name -> Bool
isDefinedD m d =
    let ts = modTSymbols m
        assDs = map dsymName . concatMap assocDSyms $ M.elems ts
    in (M.member d $ modDSymbols m) || d `elem` assDs

isDefinedT :: SymbolTable -> TName -> Bool
isDefinedT m t = M.member t $ modTSymbols m

addFDSym :: (LocSym s, DSymbol s) => s -> SymbolTable -> SymbolTable
addFDSym s = alterFDSyms (M.insert (dsymName s) (FDSym s))

addTSym :: (TSymbol s) => s -> SymbolTable -> SymbolTable
addTSym s = alterTSyms (M.insert (tsymName s) (TSym s))

getDSym :: SymbolTable -> Name -> Maybe DSym
getDSym m n = case M.lookup n $ modDSymbols m of
    Just (FDSym d) -> Just $ DSym d
    Nothing -> M.fold select Nothing $ modTSymbols m
        where
            select _ (Just d) = Just d
            select t Nothing  =
                listToMaybe . filter ((==) n . dsymName) $ assocDSyms t

getTSym :: SymbolTable -> TName -> Maybe TSym
getTSym m n = M.lookup n $ modTSymbols m

-----------------------------------------------------------------------------

-- Building the export/import list

type FilteredSyms = ([FDSym], [TSym])

getImports :: SymbolTable -> FilteredSyms
getImports = filterSyms isImported isImported

getExports :: SymbolTable -> FilteredSyms
getExports = filterSyms isExported isExported

filterSyms :: (FDSym -> Bool) -> (TSym -> Bool)
    -> SymbolTable -> FilteredSyms
filterSyms sp tp m = ( M.elems . M.filter sp $ modDSymbols m
                     , M.elems . M.filter tp $ modTSymbols m)

groupSymbols :: FilteredSyms -> ([Name], [(TName, [Name])])
groupSymbols (syms, tsyms) =
    (map dsymName syms , map ( tsymName &&& (map dsymName . assocDSyms)) tsyms)

mkImportSpec :: SymbolTable -> [ImportDecl]
mkImportSpec m =
    let modImps = groupByMN $ getImports m
        mkImport :: FilteredSyms -> ImportDecl
        mkImport syms@(ssyms, tsyms) =
            let (simps, timps) = groupSymbols syms
                -- the location of the symbols
                (mn, qmn) = case ssyms of
                    [] -> impSrc' $ head tsyms
                    (t:_) -> impSrc' t
                -- the declarations
                decls = vardecls ++ typedecls
                vardecls  = map IVar $ simps
                typedecls = map mkTDecl timps
                --TODO ConName vs VarName
                mkTDecl (TName n, []) = IAbs n
                mkTDecl (TName n, cs) = IThingWith n $ map ConName cs
            -- and finally the full import
            in ImportDecl noSrcLoc mn (isJust qmn) False Nothing qmn (Just (True, decls))
    in map mkImport modImps
    where
        -- | Group the symbols by ModuleName, essentially an extended version of
        -- groupBy for pairs
        groupByMN :: FilteredSyms -> [FilteredSyms]
        groupByMN ([], [])   = []
        groupByMN (x:xs, ys) =
            let (gxs, xs') = partition ((impSrc x ==) . impSrc) xs
                (gys, ys') = partition ((impSrc x ==) . impSrc) ys
            in (x:gxs, gys) : groupByMN (xs', ys')
        groupByMN ([], y:ys) =
            let (gys, ys') = partition ((impSrc y ==) . impSrc) ys
            in ([], y:gys) : groupByMN ([], ys')

mkExportSpec :: SymbolTable -> [ExportSpec]
mkExportSpec m =
    let (sexp, cexp) = groupSymbols $ getExports m
        --TODO ConName vs VarName
        mkCexp (TName n, [])  = EAbs (UnQual n)
        mkCexp (TName n, exs) = EThingWith (UnQual n) $ map ConName exs
    in map (EVar . UnQual) sexp ++ map mkCexp cexp

-----------------------------------------------------------------------------
