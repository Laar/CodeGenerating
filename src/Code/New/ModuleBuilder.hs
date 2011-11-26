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

module Code.New.ModuleBuilder (
    module Code.New.ModuleBuilder.Class,
--    module Code.New.ModuleBuilder.Module,
    module Code.New.ModuleBuilder.Package
) where

import Code.New.ModuleBuilder.Class
import Code.New.ModuleBuilder.Module()
import Code.New.ModuleBuilder.Package
