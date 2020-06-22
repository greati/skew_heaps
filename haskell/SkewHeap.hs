{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SkewHeap where

import Heap

-- Top down implementation of skew heaps
data SkewHeap a = Empty | Meld (SkewHeap a) a (SkewHeap a)
    deriving (Show, Eq)

instance (Ord a, Show a, Eq a) => Heap SkewHeap a where
    make_heap = Empty

    make_singleton x = Meld Empty x Empty

    find_min Empty = Nothing
    find_min (Meld _ x _ ) = Just x

    meld Empty  h2      = h2
    meld h1      Empty  = h1
    meld h1@(Meld _ x1 _) h2@(Meld Empty x2 Empty)
        | x1 <= x2  = Meld h1 x1 Empty
        | otherwise = Meld h1 x2 Empty 
    meld h1@(Meld Empty x1 Empty) h2@(Meld _ x2 _)
        | x1 <= x2  = Meld h2 x1 Empty
        | otherwise = Meld h2 x2 Empty 
    meld h1@(Meld l1 x1 r1) h2@(Meld l2 x2 r2) 
        | x1 <= x2 = Meld (Meld (meld r1 r2) x2 l2) x1 l1
        | otherwise = Meld (Meld (meld r1 r2) x1 l1) x2 l2
    
    insert x h = meld (make_singleton x) h 

    delete_min Empty = (Nothing, Empty)
    delete_min (Meld l x r) = (Just x, meld l r)

--m1 = Meld (make_singleton 50) 1 (Meld (Meld Empty 13 (make_singleton 16)) 10 (Meld (make_singleton 25) 20 Empty))
--m2 = Meld (Meld (make_singleton 40) 19 Empty) 5 (Meld (make_singleton 30) 12 (make_singleton 14))
--m3 = Meld (Meld (Meld (Meld (Meld (Meld (make_singleton 25) 20 Empty) 14 Empty) 12 (make_singleton 30)) 10 (Meld Empty 13 (make_singleton 16))) 5 (Meld (make_singleton 40) 19 Empty)) 1 (make_singleton 50)
