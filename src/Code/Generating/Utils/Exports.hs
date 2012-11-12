module Code.Generating.Utils.Exports (
    mergeExportSpec
) where

------------------------------------------------------------------------

import Data.List(union)
import Language.Haskell.Exts.Syntax

import Code.Generating.InternalUtils

------------------------------------------------------------------------
------------------------------------------------------------------------

-- | Tries to merge two `ExportSpec`s, when they can be merged return
-- `Just` the result else `Nothing`. Two `ExportSpec`s can be merged
-- when there is an `ExportSpec` that exports exactly as much as the two
-- different `ExportSpec`s would export together.
mergeExportSpec :: ExportSpec -> ExportSpec -> Maybe ExportSpec
mergeExportSpec e1 e2 = case (e1,e2) of
    -- Easy cases
    -- EVar
    (EVar q1, EVar q2) -> onEq q1 q2 e1
    -- +EAbs
    (EVar  _, EAbs  _) -> Nothing
    (EAbs q1, EAbs q2) -> onEq q1 q2 e1
    (EAbs  _, EVar  _)  -> Nothing
    -- +EThingAll
    (EThingAll q1, EThingAll q2) -> onEq q1 q2 e1
    (EVar       _, EThingAll  _) -> Nothing
    (EThingAll  _, EVar       _) -> Nothing
    (EThingAll q1, EAbs      q2) -> onEq q1 q2 e1
    (EAbs      q1, EThingAll q2) -> onEq q1 q2 e2
    -- +EModuleContents
    (EModuleContents m1, EModuleContents m2) -> onEq m1 m2 e1
    (EModuleContents  _,                  _) -> Nothing
    (                 _, EModuleContents  _) -> Nothing
    -- and now the more complicated cases EThingWith
    (EThingWith q1 c1, EThingWith q2 c2) -> onEq q1 q2 .
        EThingWith q1 $ c1 `union` c2
    (EThingWith q1  _, EAbs       q2   ) -> onEq q1 q2 e1
    (EAbs       q1   , EThingWith q2  _) -> onEq q1 q2 e2
    (EThingWith q1  _, EThingAll  q2   ) -> onEq q1 q2 e2
    (EThingAll  q1   , EThingWith q2  _) -> onEq q1 q2 e1
        -- don't attempt to merge EVars, though it might be possible
    (EThingWith  _  _, EVar        _   ) -> Nothing
    (EVar        _   , EThingWith  _  _) -> Nothing

------------------------------------------------------------------------
