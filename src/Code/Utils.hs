-----------------------------------------------------------------------------
--
-- Module      :  Code.Utils
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

module Code.Utils (
    unQual, unQualS, qual,
    unname,
    expVar, expCon, errorExp, otherwiseExp, otherwiseRhs,
    eVar, eCon, tCon,
    exportVar, exportName,

    patVar,
    tyVar,tyCon, addIOType,
    toAlt, toAlt',
    (-->>),
    (@@), (\->),
    (.$.), (...),
    (=@=),
    (/@=),
    (+@+),
    infixAppS,
    noContext,
    enumConstructor,
    sDerive,

    oneLineFun, oneLiner,
    oneTypeSig,

    emptyModule,
    emptyModule',

    moduleToModuleName, moduleNameToName, moduleToName,
    moduleNameToPath,
    addModulePragmas,

    noSrcLoc, noBinds,

    unBangType,
    unQName,
    unCName,
) where

import Data.List
import System.FilePath
import Language.Haskell.Exts.Syntax

infixl 9 @@
infixr 8 ...
infixr 5 -->>, +@+
infix  4 =@=, /@=
infixr 0 .$.

unQual :: String -> QName
unQual = UnQual . Ident

unQualS :: String -> QName
unQualS = UnQual . Symbol

qual :: ModuleName -> String -> QName
qual name = Qual name . Ident

unname :: Name -> String
unname (Ident  n) = n
unname (Symbol s) = s


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

otherwiseRhs :: Exp -> GuardedRhs
otherwiseRhs = GuardedRhs noSrcLoc  [Qualifier otherwiseExp]

exportVar :: String -> ExportSpec
exportVar = EVar . unQual

exportName :: Name -> ExportSpec
exportName = EVar . UnQual

patVar :: String -> Pat
patVar = PVar . Ident

tyVar :: String -> Type
tyVar = TyVar . Ident

tyCon :: String -> Type
tyCon = TyCon . unQual

tCon :: Name -> Type
tCon = TyCon . UnQual

addIOType :: Type -> Type
addIOType = TyApp (tyCon "IO")

toAlt :: Pat -> Exp -> Alt
toAlt p e = Alt noSrcLoc p (UnGuardedAlt e) (BDecls [])

toAlt' :: (Pat, Exp) -> Alt
toAlt' = uncurry toAlt

-- | Alias for (->) types
(-->>) :: Type -> Type -> Type
(-->>) = TyFun

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

noContext :: Context
noContext = []

enumConstructor :: Name -> QualConDecl
enumConstructor name = QualConDecl noSrcLoc [] [] $ ConDecl name []

sDerive :: String -> Deriving
sDerive n = (unQual n, [])

oneLineFun :: Name -> [Pat] -> Rhs -> Binds -> Decl
oneLineFun n ps rh bd = FunBind [Match noSrcLoc n ps Nothing rh bd]

oneLiner :: Name -> [Pat] -> Exp -> Decl
oneLiner n ps e = oneLineFun n ps (UnGuardedRhs e) noBinds

oneTypeSig :: Name -> Type -> Decl
oneTypeSig n t = TypeSig noSrcLoc [n] t

emptyModule :: ModuleName -> Module
emptyModule name = Module undefined name [] Nothing (Just []) [] []

emptyModule' :: String -> Module
emptyModule' = emptyModule . ModuleName

moduleToModuleName :: Module -> ModuleName
moduleToModuleName (Module _ name _ _ _ _ _) = name

moduleNameToName :: ModuleName -> String
moduleNameToName (ModuleName name) = name

moduleToName :: Module -> String
moduleToName = moduleNameToName . moduleToModuleName

moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName n) = foldr replace [] n
    where
        replace '.' p = pathSeparator : p
        replace  c  p = c : p

noSrcLoc :: SrcLoc
noSrcLoc = error $ "no source location"

noBinds :: Binds
noBinds = BDecls []

addModulePragmas :: [ModulePragma] -> Module -> Module
addModulePragmas prags (Module srcl name prag wt ex im decls) =
    Module srcl name (nub (prags ++ prag)) wt ex im decls


unBangType :: BangType -> Type
unBangType (UnBangedTy t) = t
unBangType (BangedTy   t) = t
unBangType (UnpackedTy t) = t

unQName :: QName -> Name
unQName (UnQual n) = n
unQName (Qual _ n) = n
unQName (Special _) = error $ "unQName: can't unQualify a special con"

unCName :: CName -> Name
unCName (VarName n) = n
unCName (ConName n) = n
