module Code.Generating.InternalUtils (
    onEq,
    mergeUpdate
) where

------------------------------------------------------------------------

import Control.Monad

------------------------------------------------------------------------

onEq :: (Eq a, MonadPlus m) => a -> a -> r -> m r
onEq a1 a2 r = if a1 == a2 then return r else mzero

-- | Merge update an element into an list. The update function is
-- applied to the new element and every element in the list until it
-- returns `Just x` then the current element is replaced. If it results
-- in `Nothing` for every element in the list then the new element is
-- added to the list.
mergeUpdate :: (a -> a -> Maybe a) -> a -> [a] -> [a]
mergeUpdate f e = go
    where
        go [] = [e]
        go (x:xs) = maybe (x:go xs) (:xs) $ f e x


------------------------------------------------------------------------
