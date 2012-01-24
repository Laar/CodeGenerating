{-# LANGUAGE
    GeneralizedNewtypeDeriving, StandaloneDeriving,
    FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances
 #-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Builder
-- Copyright   :  (c) 2011-2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This module groups the modules under the Code.Generating.ModuleBuilder
-- namespace for easy importing.
--
-----------------------------------------------------------------------------

module Code.Generating.Builder (
--    module Code.New.ModuleBuilder.Module, -- for the future
    module Code.Generating.Builder.ModuleBuilder,
    module Code.Generating.Builder.Package
) where

-----------------------------------------------------------------------------

import Code.Generating.Builder.Module() -- for the future
import Code.Generating.Builder.ModuleBuilder
import Code.Generating.Builder.Package

-----------------------------------------------------------------------------
