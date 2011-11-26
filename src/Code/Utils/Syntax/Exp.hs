-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils.Syntax.Exp
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

module Code.Utils.Syntax.Exp (
    expVar, expCon, errorExp, otherwiseExp,
    eVar, eCon,

    (@@), (\->),
    (.$.), (...),
    (=@=),
    (/@=),
    (+@+),
    infixAppS,

) where

import Language.Haskell.Exts.Syntax

import Code.Utils.No
import Code.Utils.Syntax.Names

infixl 9 @@
infixr 8 ...
infixr 5 +@+
infix  4 =@=, /@=
infixr 0 .$.

expVar :: String -> Exp
expVar = Var . unQual

eVar :: Name -> Exp
eVar = Var . UnQual

expCon :: String -> Exp
expCon = Con . unQual

eCon :: Name -> Exp
eCon = Con . UnQual

errorExp :: String -> Exp
errorExp = App (expVar "error") . Lit . String

otherwiseExp :: Exp
otherwiseExp = expVar "otherwise"

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
infixAppS inf ex1 ex2 = InfixApp ex1 (QVarOp $ unQualS inf) ex2
