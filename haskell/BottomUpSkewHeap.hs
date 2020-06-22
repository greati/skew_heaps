{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BottomUpSkewHeap where

import Heap

-- BottomUp implementation of skew heaps
data Crumb a = LeftCrumb a (BottomUpSkewHeap a) | RightCrumb a (BottomUpSkewHeap a)
    deriving (Show, Eq)
type Breadcrumbs a = [Crumb a]

-- The idea here is to store the current tree plus a zipper of the rightmost node
data BottomUpSkewHeap a = Empty | Meld (BottomUpSkewHeap a) a (BottomUpSkewHeap a) 
        (BottomUpSkewHeap a, Breadcrumbs a)
    deriving (Show, Eq)

instance (Ord a) => Heap BottomUpSkewHeap a where
    make_heap = Empty 

    make_singleton x = Meld Empty x Empty (Empty, [])

    find_min Empty = Nothing
    find_min (Meld _ x _ _ ) = Just x

    meld h1     Empty   = h1
    meld Empty  h2      = h2
    meld h1@(Meld l1 x1 r1 (rm1@(Meld rml1 rmx1 rmr1 k), (RightCrumb s1 ll1):b1s)) 
         h2@(Meld l2 x2 r2 ((Meld rml2 rmx2 rmr2 w), (RightCrumb s2 ll2):b2s))
        | rmx1 <= rmx2 = meld (Meld (newleft) s1 ll1 (Meld ll1 s1 rm1 k, b1s)) Empty
        | otherwise = undefined
        where newleft = Meld (Meld rmr2 rmx2 rml2 w) rmx1 rml1 k

    insert = undefined

    delete_min = undefined
