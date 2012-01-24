-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Exp
-- Copyright   :  (c) 2011 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Code.Generating.Utils.Syntax.Exp (
    var', con', errorExp, otherwiseExp,
    var, con,

    (@@), (\->),
    (.$.), (...),
    (=@=),
    (/@=),
    (+@+),
    infixAppS,

) where

-----------------------------------------------------------------------------

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.No
import Code.Generating.Utils.Syntax.Names

-----------------------------------------------------------------------------

infixl 9 @@
infixr 8 ...
infixr 5 +@+
infix  4 =@=, /@=
infixr 0 .$.

var' :: String -> Exp
var' = Var . unQual'

var :: Name -> Exp
var = Var . UnQual

con' :: String -> Exp
con' = Con . unQual'

con :: Name -> Exp
con = Con . UnQual

errorExp :: String -> Exp
errorExp = App (var' "error") . Lit . String

otherwiseExp :: Exp
otherwiseExp = var' "otherwise"

-- Opperators

(@@) :: Exp -> Exp -> Exp
(@@) = App

(.$.) :: Exp -> Exp -> Exp
(.$.) = infixAppS "$"

(...) :: Exp -> Exp -> Exp
(...) = infixAppS "."

(=@=), (/@=) :: Exp -> Exp -> Exp
(=@=) = infixAppS "=="
(/@=) = infixAppS "/="

(+@+) :: Exp -> Exp -> Exp
(+@+) = infixAppS "++"

(\->) :: [Pat] -> Exp -> Exp
(\->) = Lambda noSrcLoc

infixAppS :: String -> Exp -> Exp -> Exp
infixAppS inf ex1 ex2 = InfixApp ex1 (QVarOp $ unQualSym' inf) ex2

-----------------------------------------------------------------------------
