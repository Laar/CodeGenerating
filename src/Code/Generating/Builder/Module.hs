{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Builder.Module
-- Copyright   :  (c) 2011-2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Instance of `BuildableModule` for `Module`
--
-----------------------------------------------------------------------------

module Code.Generating.Builder.Module (

) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax

import Code.Generating.Builder.ModuleBuilder

-----------------------------------------------------------------------------

instance BuildableModule Module where
    addBExport e (Module sl mn mp wt exs im dc) =
        Module sl mn mp wt exs' im dc
        where
            exs' = case exs of
                Just es -> Just $ e : es
                Nothing -> Nothing -- TODO what should be done here?
    addBDecls d  (Module sl mn mp wt exs im dc) = Module sl mn mp wt exs im (d ++ dc)
    addBImport i (Module sl mn mp wt exs im dc) = Module sl mn mp wt exs (addImportD i im) dc
    addBPragma p (Module sl mn mp wt exs im dc) = Module sl mn (mp') wt exs im dc
        where mp' = singleToModulePragma p : mp -- TODO, improve, undup?
    getBImport i (Module _  _  _  _  _   im _ ) = filter (\im' -> importModule im' == i) im
    hasBPragma p (Module _  _  mp _  _   _  _ ) =
        any (pragmaExtends p) $ concatMap modulePragmaToSingles mp

-----------------------------------------------------------------------------

addImportD :: ImportDecl -> [ImportDecl] -> [ImportDecl]
addImportD i = go
    where
        go []     = [i]
        go (j:js) = case tryMergeDecls i j of
            Nothing -> j : go js
            Just i' -> i' : js


tryMergeDecls :: ImportDecl -> ImportDecl -> Maybe ImportDecl
tryMergeDecls (ImportDecl _ _ _ s _ _ _ ) _ | s = Nothing
tryMergeDecls _ (ImportDecl _ _ _ s _ _ _ ) | s = Nothing
tryMergeDecls (ImportDecl _ mn1 q1 _ p1 a1 _) (ImportDecl _ mn2 q2 _ p2 a2 _)
    | mn1 /= mn2 || a1 /= a2 || q1 /= q2 || p1 /= p2 = Nothing
tryMergeDecls i@(ImportDecl _ _ _ _ _ _ Nothing) _ = Just i
tryMergeDecls _ i@(ImportDecl _ _ _ _ _ _ Nothing) = Just i
tryMergeDecls (ImportDecl l mn q s p a (Just (h1, sp1))) (ImportDecl _ _ _ _ _ _ (Just (h2, sp2)))
    | h1 == h2  = Just $ ImportDecl l mn q s p a (Just (h1, sp1 ++ sp2))
    | otherwise = Nothing -- Just to be sure

-----------------------------------------------------------------------------
