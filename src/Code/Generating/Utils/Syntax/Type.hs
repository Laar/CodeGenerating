-----------------------------------------------------------------------------
--
-- Module      :  Code.Generating.Utils.Syntax.Type
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

module Code.Generating.Utils.Syntax.Type (
    tyCon, tyCon',
    tyVar', addIOType,
    (-->>),

    specializeType,
    despecializeType,
    traverseType, traverseTypeM,
    foldType,
) where

-----------------------------------------------------------------------------

import Control.Applicative(Applicative(..), (<$>))
import Control.Monad.Identity
import Data.Monoid(Monoid(..))

import Language.Haskell.Exts.Syntax

import Code.Generating.Utils.Syntax.Names

-----------------------------------------------------------------------------

infixr 5 -->>

tyVar' :: String -> Type
tyVar' = TyVar . Ident

tyCon' :: String -> Type
tyCon' = TyCon . unQual'

tyCon :: Name -> Type
tyCon = TyCon . UnQual

addIOType :: Type -> Type
addIOType = TyApp (tyCon' "IO")

-- | Alias for (->) types
(-->>) :: Type -> Type -> Type
(-->>) = TyFun

-----------------------------------------------------------------------------

-- | Changes occurences of
-- > TyApp (TyCon (Special ListCon)) t
-- to @TyList t@. It also switches `TupleCon`s to TyTuple if its size matches
-- the number of `TyApp`s. If there are more `TyApp`s than it's size it
-- errors. Every occurence of `FunCon` applied to two types is resolved into
-- TyFun between it's arguments
-- Idempotent.
specializeType :: Type -> Type
specializeType = traverseType go
    where
        go (TyApp (TyCon (Special ListCon)) t) = Just . TyList $ specializeType t
        go (TyApp (TyApp (TyCon (Special FunCon)) t1) t2)
            = Just $ TyFun (specializeType t1) (specializeType t2)
        go t@(TyApp _ _) = case box t of
            Just (ts, b, n)
                | n == 0 -> Just . TyTuple b . reverse $ map specializeType ts
                | n < 0  -> error $ "Tuple size underflow"
            -- '_' as it needs also to be done in case of an underapplied tuple
            _            -> Nothing
            where
                box :: Type -> Maybe ([Type], Boxed, Int)
                box (TyCon (Special (TupleCon b i))) = Just ([], b, i)
                box (TyApp t1' t2') = case box t1' of
                    Nothing -> Nothing
                    Just (_,  _, 0) -> error $ "Tuple size underflow"
                    Just (ts, b, i) -> Just (specializeType t2' : ts, b, i - 1)
                box _             = Nothing
        go _ = Nothing

-----------------------------------------------------------------------------

-- Replaces `TyList` and `TyTuple` by there SpecialCon variants. It's the
-- practical inverse of `specializeType` (`TyFun` is not mapped).
-- Idempotent
despecializeType :: Type -> Type
despecializeType = traverseType go
    where
        go (TyList t)    = Just $ TyCon (Special ListCon) `TyApp` despecializeType t
        go (TyTuple b t) = Just . foldl TyApp tupCon $ map despecializeType t
            where tupCon = TyCon . Special $ TupleCon b (length t)
        go _             = Nothing

-----------------------------------------------------------------------------

-- | Pure version of `traverseTypeM`.
traverseType :: (Type -> Maybe Type) -> Type -> Type
traverseType f t = runIdentity $ traverseTypeM (return . f) t

-- | Maps a function over a `Type`tree. For a node @t :: `Type`@ in the tree
-- it will try to replace it with the result of the mapping function. If this
-- fails due to a `Nothing` the leafs of this node will be replaced by the
-- mapped versions of them, thereby applying the function to every `Type` in
-- the original `Type`tree.
traverseTypeM :: (Applicative m, Monad m) => (Type -> m (Maybe Type)) -> Type ->  m Type
traverseTypeM f t = f t >>= \mt' -> case mt' of
    Just t' -> return $ t'
    Nothing -> case t of
        TyForall mtb c t' -> TyForall mtb c <$> traverseTypeM f t'
        TyFun     t1 t2   -> TyFun     <$> traverseTypeM f t1 <*> traverseTypeM f t2
        TyTuple b ts      -> TyTuple b <$> mapM (traverseTypeM f) ts
        TyList    t'      -> TyList    <$> traverseTypeM f t'
        TyApp     t1 t2   -> TyApp     <$> traverseTypeM f t1 <*> traverseTypeM f t2
        TyParen   t'      -> TyParen   <$> traverseTypeM f t'
        TyInfix   t1 c t2 -> TyInfix   <$> traverseTypeM f t1 <*> pure c <*> traverseTypeM f t2
        TyKind    t' k    -> TyKind    <$> traverseTypeM f t' <*> pure k
        _                 -> return t

-----------------------------------------------------------------------------

-- | Folds a `Type` into a value in top down style. That is for each type
-- the function is applied, if it results in @Just t@ then @t@ will be the
-- result, otherwise it will fold its `Type`leafs combining them.
-- A monadic version can be created by using a `Writer` with `traverseTypeM`.
foldType :: Monoid m => (Type -> Maybe m) -> Type -> m
foldType f t = case f t of
    Just t' -> t'
    Nothing -> case t of
        TyForall _ _ t' -> foldType f t'
        TyFun    t1 t2  -> foldType f t1 `mappend` foldType f t2
        TyTuple  _ ts   -> mconcat $ map (foldType f) ts
        TyList   t'     -> foldType f t'
        TyApp    t1 t2  -> foldType f t1 `mappend` foldType f t2
        TyParen  t'     -> foldType f t'
        TyInfix  t1 _ t2 -> foldType f t1 `mappend` foldType f t2
        TyKind   t' _   -> foldType f t'
        _               -> mempty

-----------------------------------------------------------------------------
