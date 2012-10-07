-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This namespace defines a large amount of helper function for generating
-- haskell-src-exts syntax trees. All these functions are exported from here.
--
-- There are several functions combining constructors into shorter forms,
-- e.g. `tyCon` which combines `TyCon` and `UnQual`. All these short hands
-- use the convention that the name is the same as the last applied
-- constructor. Furthermore for many of these functions there are two
-- an unprimed on of type `Name -> a` and a primed one of type `String -> a`.
-- The unprimed one does not exist when it would be a single type constructor
-- (What is the need of `tyVar` when there is the same `TyVar`).
--
-- Furthermore, every function here uses unqualified names unless otherwise
-- stated.
-----------------------------------------------------------------------------

module Code.Generating.Utils (

    module Code.Generating.Utils.Imports,
    module Code.Generating.Utils.No,
    module Code.Generating.Utils.Module,
    module Code.Generating.Utils.Syntax,

    module Code.Generating.Utils.Unsorted,
) where

-----------------------------------------------------------------------------

import Code.Generating.Utils.Imports
import Code.Generating.Utils.Module
import Code.Generating.Utils.No
import Code.Generating.Utils.Syntax
import Code.Generating.Utils.Unsorted

-----------------------------------------------------------------------------
