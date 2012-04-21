-----------------------------------------------------------------------------
--
-- Module      :  Main (Tests.hs)
-- Copyright   :  (c) 2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Test runner
--
-----------------------------------------------------------------------------

module Main (main) where

------------------------------------------------------------------------

import Test.Framework (Test, defaultMain)

------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
    ]
