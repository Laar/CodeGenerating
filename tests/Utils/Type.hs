-----------------------------------------------------------------------------
--
-- Module      :  Utils.Type
-- Copyright   :  (c) 2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Utils.Type (typeTest) where

------------------------------------------------------------------------

import Data.Monoid(mempty)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Instances.HaskellSrcExts.Generators
import Test.QuickCheck.Instances.HaskellSrcExts.Generators.Junk98

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils

------------------------------------------------------------------------

typeTest :: Test
typeTest = testGroup "Utils.Type"
    [ testProperty "Specialize Idempotent"      prop_spIdempotent
    , testProperty "Despecialize Idempotent"    prop_dspIdempotent
    , testProperty "Specialize Inverse"         prop_spInverse
    , testProperty "Despecialize Inverse"       prop_dspInverse
    , testProperty "Identity traversal"         prop_traverseTypeID
    , testProperty "Mempty fold"                prop_foldTypeMempty
    ]

------------------------------------------------------------------------

forAllShrinkType :: Testable prop => (Type -> prop) -> Property
forAllShrinkType = forAllShrink (typeJunkGen Ascii) shrinkType

------------------------------------------------------------------------
-- Idempotency
prop_spIdempotent :: Property
prop_spIdempotent = forAllShrinkType $ \t ->
    let st = specializeType t
    in specializeType st == st

prop_dspIdempotent :: Property
prop_dspIdempotent = forAllShrinkType $ \t ->
    let dt = despecializeType t
    in despecializeType dt == dt

------------------------------------------------------------------------
-- Inversing properties, applying despecializeType and specializeType
-- one after the other should not modify the type if it's in the correct
-- form.
prop_spInverse :: Property
prop_spInverse = forAllShrinkType $ \t ->
    let st = specializeType t
    in st == specializeType (despecializeType st)

prop_dspInverse :: Property
prop_dspInverse = forAllShrinkType $ \t ->
    -- a specialize is needed as it transforms FunCon and despecialize
    -- doesn't.
    let st = despecializeType $ specializeType t
    in st == despecializeType (specializeType st)

------------------------------------------------------------------------

prop_traverseTypeID :: Property
prop_traverseTypeID = forAllShrinkType $ \t ->
    traverseType (const Nothing) t == t

------------------------------------------------------------------------

prop_foldTypeMempty :: Property
prop_foldTypeMempty = forAllShrinkType $ \t ->
    foldType (const Nothing) t == (mempty :: [Int])
