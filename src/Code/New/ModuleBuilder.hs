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
) where

import Code.New.ModuleBuilder.Class
import Code.New.ModuleBuilder.Module()
