{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Heap where

-- Common signature for heaps
class (Ord a, Show a, Eq a) => Heap m a where
    make_heap :: m a
    make_singleton :: a -> m a
    find_min :: m a -> Maybe a
    meld :: m a -> m a -> m a
    insert :: a -> m a -> m a
    delete_min :: m a -> (Maybe a, m a)

