module Code.Generating.Utils.Imports (
    isSimpleImportDecl, combineImportDecl, addImportDecl,

    ImportList(..),

    importSpecUnion, importSpecIntersect,
    ImportSpecDiff(..), importSpecDifference,

    addIncludeSpec, mergeIncludeSpec,
    mergeHidingSpec, mergeHideIncludeSpec,
) where

-----------------------------------------------------------------------

import Data.List(union, intersect, nub, (\\), foldl')
import Data.Maybe(isJust, fromJust, isNothing)
import Data.Monoid

import Language.Haskell.Exts.Syntax
import Code.Generating.Utils.Syntax.Names
import Code.Generating.InternalUtils

-----------------------------------------------------------------------

-- | Checks that an `ImportDecl` is 'simple', thus it's not imported
-- with a specific source or package name.
isSimpleImportDecl :: ImportDecl -> Bool
isSimpleImportDecl i = not (importSrc i) && isNothing (importPkg i)

-----------------------------------------------------------------------

-- | Tries to combine two `ImportDecl`s. When it succeeds it will
-- return `Just i` with `i` the equivalent `ImportDecl`, when this is
-- not possible it will return `Nothing`.
combineImportDecl :: ImportDecl -> ImportDecl -> Maybe ImportDecl
combineImportDecl i1@(ImportDecl _ n1 q1 _ si1 _ a1 s1) i2@(ImportDecl _ n2 q2 _ si2 _ a2 s2)
    | isSimpleImportDecl i1 && isSimpleImportDecl i2
      && n1 == n2 && q1 == q2 && a1 == a2 && si1 == si2
    = case (s1, s2) of
        (Nothing, _      ) -> Just i1
        (_      , Nothing) -> Just i2
        (Just (True, is1), Just (True, is2))
            -- collapsing is not correct here and should not happen
            -> Just $ i1{importSpecs = Just (True, mergeHidingSpec is1 is2)}
        (Just (True, is1), Just (False, is2))
            -> case mergeHideIncludeSpec is1 is2 of
                Nothing  -> Nothing
                Just is' -> Just i1{importSpecs = collapseEmpty (True, is')}
        (Just (False, is1), Just (True, is2))
            -> case mergeHideIncludeSpec is2 is1 of
                Nothing  -> Nothing
                Just is' -> Just i1{importSpecs = collapseEmpty (True, is')}
        (Just (False, is1), Just (False, is2))
            -> Just $ i1{importSpecs = Just (False, mergeIncludeSpec is1 is2)}
    | otherwise = Nothing
    where
        -- for collapsing empty hiding statements into full imports.
        collapseEmpty (_, []) = Nothing
        collapseEmpty i       = Just i

-----------------------------------------------------------------------

-- | Adds an `ImportDecl` to a list and when possible merging the
-- two `ImportDecl`s in stead of adding it to the end.
addImportDecl :: ImportDecl -> [ImportDecl] -> [ImportDecl]
addImportDecl = mergeUpdate combineImportDecl

-----------------------------------------------------------------------

-- | `ImportList` newtype, using addImportDecl as basis for a `Monoid`
-- instance which merges imports.
newtype ImportList = ImportList { toIDecls :: [ImportDecl] }
    deriving(Eq, Ord, Show)

instance Monoid ImportList where
    mempty = ImportList []
    il1 `mappend` il2 = ImportList $ foldr (addImportDecl) (toIDecls il1) (toIDecls il2)

-----------------------------------------------------------------------

mergeHidingSpec :: [ImportSpec] -> [ImportSpec] -> [ImportSpec]
mergeHidingSpec is1 is2
    = [fromJust i
         | i1 <- is1
         , i2 <- is2
         , let i = importSpecIntersect i1 i2
         , isJust i]

addIncludeSpec :: ImportSpec -> [ImportSpec] -> [ImportSpec]
addIncludeSpec = mergeUpdate importSpecUnion

mergeIncludeSpec :: [ImportSpec] -> [ImportSpec] -> [ImportSpec]
mergeIncludeSpec = foldl' (flip addIncludeSpec)

mergeHideIncludeSpec :: [ImportSpec] -> [ImportSpec] -> Maybe [ImportSpec]
mergeHideIncludeSpec hd inc = go hd  (Just [])
    where
        go _      Nothing = Nothing
        go []     rs = rs
        go (h:hs) jrs@(Just rs)
                = case foldr (foldHide h) (Just Keep) inc of
            Nothing          -> go hs Nothing
            Just Keep        -> go hs      (Just $ h:rs)
            Just Drop        -> go hs      jrs
            Just (Change h') -> go (h':hs) jrs
        foldHide :: ImportSpec -> ImportSpec
                 -> Maybe ImportSpecDiff -> Maybe ImportSpecDiff
        foldHide h i (Just Keep) = importSpecDifference h i
        foldHide _ _ r           = r

-----------------------------------------------------------------------

-- Tries to combine two `ImportSpec`s onto one, which when part of a
-- hiding import (i.e. import A hiding (a,b)), should result in the
-- same import as having to seperate hiding imports.
importSpecIntersect :: ImportSpec -> ImportSpec -> Maybe ImportSpec
importSpecIntersect i1 i2 = case (i1, i2) of
-- x,x
    (IVar s1 n1      , IVar s2 n2)      -> onEq (s1, n1) (s2, n2) i2
    (IAbs n1         , IAbs n2)         -> onEq n1 n2 i2
    (IThingAll n1    , IThingAll n2)    -> onEq n1 n2 i2
    (IThingWith n1 p1, IThingWith n2 p2)
        | n1 /= n2  -> Nothing
        | otherwise -> Just . IThingWith n1 $ p1 `intersect` p2
-- IVar,* and *,IVar
    (IVar NoNamespace n1, IThingWith _ ps)
        | n1 `elem` (map unCName ps) -> Just i1
        | otherwise                  -> Nothing
    (IThingWith _ ps, IVar NoNamespace n2)
        | n2 `elem` (map unCName ps) -> Just i2
        | otherwise                  -> Nothing
-- IAbs,* and *,IAbs
    (IAbs n1        , IThingAll n2   ) -> onEq n1 n2 i1
    (IThingAll n1   , IAbs n2        ) -> onEq n1 n2 i2
    (IAbs n1        , IThingWith n2 _) -> onEq n1 n2 i1
    (IThingWith n1 _, IAbs n2        ) -> onEq n1 n2 i2
-- IThingAll,* and *,IThingAll
    (IThingAll n1   , IThingWith n2 _) -> onEq n1 n2 i2
    (IThingWith n1 _, IThingAll n2   ) -> onEq n1 n2 i1
-- other cases (IVar,IAbs), (IVar, IThingAll)
    (_              ,_               ) -> Nothing

-----------------------------------------------------------------------

-- Tries to combine two `ImportSpec`s into one which, when part of a
-- restricted import (i.e. import A (a,b)), should result in the same
-- import as having two seperate imports.
importSpecUnion :: ImportSpec -> ImportSpec -> Maybe ImportSpec
importSpecUnion i1 i2 = case (i1, i2) of
-- x,x
    (IVar s1 n1      , IVar s2 n2)      -> onEq (s1, n1) (s2, n2) i2
    (IAbs n1         , IAbs n2)         -> onEq n1 n2 i2
    (IThingAll n1    , IThingAll n2)    -> onEq n1 n2 i2
    (IThingWith n1 p1, IThingWith n2 p2)
        | n1 /= n2  -> Nothing
        | otherwise -> Just . IThingWith n1 $ p1 `union` p2
-- IVar,* and *,IVar
    (IVar NoNamespace n1, IThingWith _ ps)
        | n1 `elem` (map unCName ps) -> Just i2
        | otherwise                  -> Nothing
    (IThingWith _ ps, IVar NoNamespace n2)
        | n2 `elem` (map unCName ps) -> Just i1
        | otherwise                  -> Nothing
-- IAbs,* and *,IAbs
-- TODO: think about constructor hiding
    (IAbs n1        , IThingAll n2   ) -> onEq n1 n2 i2
    (IThingAll n1   , IAbs n2        ) -> onEq n1 n2 i1
    (IAbs n1        , IThingWith n2 _) -> onEq n1 n2 i2
    (IThingWith n1 _, IAbs n2        ) -> onEq n1 n2 i1
-- IThingAll,* and *,IThingAll
    (IThingAll n1   , IThingWith n2 _) -> onEq n1 n2 i1
    (IThingWith n1 _, IThingAll n2   ) -> onEq n1 n2 i2
-- other cases (IVar,IAbs), (IVar, IThingAll)
    (_              ,_               ) -> Nothing

-----------------------------------------------------------------------

-- | Merge result for ImportSpecs for the difference of two ImportSpecs
data ImportSpecDiff
    = Keep
    | Drop
    | Change ImportSpec

-- | Tries to merge a hiding ImportSpec with an including ImportSpec.
-- The result is `Nothing` when it's not possible to combine it into
-- one import that is equivalent to the two seperate imports.
importSpecDifference :: ImportSpec -> ImportSpec
    -> Maybe ImportSpecDiff
importSpecDifference i1 i2 = case (i1, i2) of
    -- duplicates are removed
    -- IVar
        -- IVar
    (IVar s1 n1, IVar s2 n2) -> nEq (s1, n1) (s2, n2)
        -- IAbs
    (IVar _ _, IAbs  _) -> keep
        -- IThingAll -> undecidable
            -- The var could be a class methode or record and then it
            -- should not be hidden. When it's not it should be hidden,
            -- therefore it's undecidable.
    (IVar _ _ , IThingAll     _) -> Nothing
        -- IThingWith ->
    (IVar NoNamespace n1, IThingWith _ ps)
        | n1 `elem` map unCName ps -> Just Drop
        -- Not requested by the include
        | otherwise                -> keep
    -- IAbs
        -- IVar
    (IAbs  _, IVar NoNamespace _) -> keep
        -- IAbs -> undecidable
            -- The hiding could hide in addition to the abstract
            -- datatype also its constructor with the same name.
            -- When a constructor is hidden this combination can't be
            -- packed into a single ImportSpec
    (IAbs n1, IAbs n2) -> undecideNameEq n1 n2
        -- IThingAll
    (IAbs n1, IThingAll n2   ) -> nEq n1 n2
        -- IThingWith -> undecidable
            -- As with IAbs there might the constructor might be hidden
    (IAbs n1, IThingWith n2 _) -> undecideNameEq n1 n2
    -- IThingAll
        -- IVar -> undecidable due to record/typeclass method hiding
    (IThingAll _, IVar _ _) -> Nothing
        -- IAbs -> (non possible)
            -- Not possible in one line to hide everything but the
            -- abstract datatype. For the same datatype.
    (IThingAll n1, IAbs n2) -> undecideNameEq n1 n2
        -- IThingAll -> i1
    (IThingAll n1, IThingAll n2) -> nEq n1 n2
        -- IThingWith -> (non possible) see IAbs
    (IThingAll n1, IThingWith n2 _) -> undecideNameEq n1 n2
    -- IThingWith
        -- IVar -> undecidable
            -- As with the reverse case, it could be a class method or
            -- record that is implicitly requested
    (IThingWith _ ps, IVar NoNamespace n2)
        | n2 `elem` map unCName ps -> Just . Change
            . IThingWith n2 $ filter ((/=) n2 . unCName) ps
        | otherwise    -> Nothing
        -- IAbs -> not possible
    (IThingWith n1 _, IAbs n2) -> undecideNameEq n1 n2
        -- IAll
    (IThingWith n1 _, IThingAll n2)
        -> Just $ if n1 == n2
            then Drop
            else Keep
        -- IThingWith -> undecidable
            -- can be decided when there is more included than excluded
    (IThingWith n1 p1, IThingWith n2 p2)
        | n1 /= n2 -> Just Keep
        | null $ (nub p1) \\ p2 -> Just Drop
        | otherwise -> Nothing
    where
        -- non mergable -> keep the hiding
        keep = Just Keep
        -- undecidable in case of equal names
        undecideNameEq n1 n2 = if n1 == n2 then Nothing else keep
        nEq :: Eq a => a -> a -> Maybe ImportSpecDiff
        nEq n1 n2 = Just $ if n1 == n2 then Drop else Keep

-----------------------------------------------------------------------
