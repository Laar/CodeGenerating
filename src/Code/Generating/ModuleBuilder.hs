{-# LANGUAGE
    GeneralizedNewtypeDeriving, StandaloneDeriving,
    FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances
 #-}
-----------------------------------------------------------------------------
--
-- Module      :  Code.New.ModuleBuilder
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

module Code.Generating.ModuleBuilder (
    module Code.Generating.ModuleBuilder.Class,
--    module Code.New.ModuleBuilder.Module, -- for the future
    module Code.Generating.ModuleBuilder.Package
) where

import Code.Generating.ModuleBuilder.Class
import Code.Generating.ModuleBuilder.Module() -- for the future
import Code.Generating.ModuleBuilder.Package
