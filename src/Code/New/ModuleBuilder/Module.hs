{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.New.ModuleBuilder.Module
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

module Code.New.ModuleBuilder.Module (

) where

import Data.List(find)
import Language.Haskell.Exts.Syntax
import Code.Utils

import Code.New.ModuleBuilder.Class

instance BuildableModule Module where
    addBExport e (Module sl mn mp wt exs im dc) =
        Module sl mn mp wt exs' im dc
        where
            exs' = case exs of
                Just es -> Just $ e : es
                Nothing -> Nothing -- TODO what should be done here?
    addBDecls d  (Module sl mn mp wt exs im dc) = Module sl mn mp wt exs im (d ++ dc)
    addBImport i (Module sl mn mp wt exs im dc) = Module sl mn mp wt exs (i:im) dc -- Can we merge?
    addBPragma p (Module sl mn mp wt exs im dc) = Module sl mn (p:mp) wt exs im dc
    getBImport i (Module _  _  _  _  _   im _ ) = find (\im' -> importModule im' == i) im
    hasBPragma p (Module _  _  mp _  _   _  _ ) = p `elem` mp -- TODO is == defined right for this
