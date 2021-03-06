{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BottomUpSkewHeap where

import Heap
import Data.Sequence as Seq
import Data.Tree as DTree

{- A different implementation of a binary tree. Instead of 
 - describing it as B x A x B, i.e., left subtree, value and right subtree,
 - describe it as a list of (B, A), where [] is the empty tree
 - and (L, x):R is the binary tree such that x is the
 - root value, L is the left subtree and R is the right subtree. We
 - also assume the form T:(L, x) as a way to access the last element
 - in the rightmost path of the tree.
 -}
data BottomUpSkewHeap a = Tree (Seq (BottomUpSkewHeap a, a))
    deriving (Eq)

{- Auxiliar meld operation implementing tail recursion.
 - For a detailed explation on this, check the dissertation
 - "Data Structures and Amortized Complexity in a Functional Setting",
 - by Berry Schoenmakers.
 -}
meld' :: Ord a => BottomUpSkewHeap a -> BottomUpSkewHeap a -> BottomUpSkewHeap a -> BottomUpSkewHeap a
meld' (Tree Seq.Empty) (Tree Seq.Empty) z = z
meld' x@(Tree (xu :|> (xt, xa))) y@(Tree Seq.Empty) z@(Tree zs) = meld' (Tree xu) y (Tree ((xt, xa) :<| zs))
meld' x@(Tree Seq.Empty) y@(Tree (yu :|> (yt, ya))) z@(Tree zs) = meld' (Tree yu) x (Tree ((yt, ya) :<| zs))
meld' x@(Tree (xu :|> (xt@(Tree xts), xa))) y@(Tree (yu :|> (yt@(Tree yts), ya))) z
    | ya <= xa  = meld' (Tree xu) y (Tree ((z, xa) :<| xts))
    | otherwise = meld' (Tree yu) x (Tree ((z, ya) :<| yts))

instance (Ord a, Show a, Eq a) => Show (BottomUpSkewHeap a) where
    show (Tree Seq.Empty) = []
    show h = (drawTree . toStringDataTree) $ h

instance (Ord a, Show a, Eq a) => Heap BottomUpSkewHeap a where

    make_heap = Tree (Seq.Empty)

    make_singleton x = Tree $ (make_heap, x) :<| Seq.Empty

    find_min (Tree Seq.Empty) = Nothing
    find_min (Tree ((_, x) :<| _)) = Just x

    meld x y = meld' x y (Tree Seq.Empty)

    insert x y = meld (make_singleton x) y
    
    delete_min (Tree Seq.Empty) = (Nothing, Tree Seq.Empty)
    delete_min (Tree ((x, ta) :<| y)) = (Just ta, meld x (Tree y))

    toStringDataTree x@(Tree ((xl, xa) :<| xr)) = Node (show xa) [toStringDataTree xl, toStringDataTree (Tree xr)]
    toStringDataTree (Tree Seq.Empty) = Node "[]" []


