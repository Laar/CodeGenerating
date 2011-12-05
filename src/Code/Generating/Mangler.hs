-----------------------------------------------------------------------------
--
-- Module      :  Code.Mangler
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

module Code.Generating.Mangler (
--    hexify, hexifyAll,

    PPDirective,
    addafterImport,

    Macro,
    appendMacros,
    ModuleSource,
) where

import Data.List


type ModuleSource = String

-- | hexify a string, under the condition that it only consists of digit
--hexify :: String -> String
--hexify xs | all isDigit xs = let val = read xs :: Integer
--                                 printLength = length $ showHex val []
--                             in "0x" ++ showHex' (max 4 printLength) val
--          | otherwise      = xs
--
---- | apply hexify to all words
--hexifyAll :: ModuleSource -> ModuleSource
--hexifyAll = unlines . map (unwords . map hexify .words) . lines

type PPDirective = String

-- | Add the given preprocessor-directives after the last import
-- this function depends on there being a white line after the last
-- import statement so, import [\n](lots of functions, etc, but no \n) \n\n
addafterImport :: [PPDirective] -> ModuleSource -> ModuleSource
addafterImport ppds msc=
    let (postImport, rimports) = break isImportLine . reverse . lines $ msc
        imports = unlines . reverse $ rimports
        (lastImport,moduleBody ) = break isEmptyLine . reverse $ postImport
    in imports ++ (unlines lastImport) -- the import part
        ++ formatedPPDs
        ++ '\n' : (unlines moduleBody) -- the body part
        where isImportLine = isPrefixOf "import "
              isEmptyLine  = all (\x-> x `elem` " \t")
              formatedPPDs = concatMap ('\n':) ppds


type Macro = String

appendMacros :: [Macro] -> ModuleSource -> ModuleSource
appendMacros macros msc =  msc ++ ('\n' : concatMap ('\n':) macros)
