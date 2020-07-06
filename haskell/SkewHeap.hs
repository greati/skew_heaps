{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SkewHeap where

import Heap
import Data.Tree as DTree

-- Top down implementation of skew heaps
data SkewHeap a = Empty | Tree (SkewHeap a) a (SkewHeap a)
    deriving (Eq)

instance (Ord a, Show a, Eq a) => Show (SkewHeap a) where
    show Empty = []
    show h = (drawTree . toStringDataTree) $ h

instance (Ord a, Show a, Eq a) => Heap SkewHeap a where
    make_heap = Empty

    make_singleton x = Tree Empty x Empty

    find_min Empty = Nothing
    find_min (Tree _ x _ ) = Just x

    meld Empty  y       = y
    meld x      Empty   = x
    meld x@(Tree l1 a1 r1) y@(Tree l2 a2 r2) 
        | a1 <= a2  = Tree (meld r1 y) a1 l1
        | otherwise = Tree (meld x r2) a2 l2
    
    insert x h = meld (make_singleton x) h 

    delete_min Empty = (Nothing, Empty)
    delete_min (Tree l x r) = (Just x, meld l r)

    toStringDataTree x@(Tree xl xa xr) = Node (show xa) [toStringDataTree xl, toStringDataTree xr]
    toStringDataTree Empty = Node "[]" []

m1 = Tree (make_singleton 50) 1 (Tree (Tree Empty 13 (make_singleton 16)) 10 (Tree (make_singleton 25) 20 Empty))
m2 = Tree (Tree (make_singleton 40) 19 Empty) 5 (Tree (make_singleton 30) 12 (make_singleton 14))
m3 = Tree (Tree (Tree (Tree (Tree (Tree (make_singleton 25) 20 Empty) 14 Empty) 12 (make_singleton 30)) 10 (Tree Empty 13 (make_singleton 16))) 5 (Tree (make_singleton 40) 19 Empty)) 1 (make_singleton 50)
