module BinarySearchTree where

import BinaryTree

type BinarySearchTree a = BinaryTree a

treeIsValid :: Ord a => BinarySearchTree a -> Bool
treeIsValid x = case x of
   Null -> True
   Node a' Null Null -> True
   Node l1 l2 l3
             |isMonotonicallyIncreasing (treeFlatten (Node l1 l2 l3)) -> True
             |otherwise -> False

                  where isMonotonicallyIncreasing :: (Ord a) => [a]  -> Bool
                        isMonotonicallyIncreasing list = case list of
                                     []         -> True
                                     [x]        -> True
                                     i1: i2: is | i1 <= i2  -> isMonotonicallyIncreasing (i2: is)
                                                | otherwise -> False

treeMinimum :: BinarySearchTree a -> a
treeMinimum x = case x of
            Null -> undefined
            Node a Null Null -> a
            Node _ b _ -> treeMinimum b

treeMaximum :: BinarySearchTree a -> a
treeMaximum x = case x of
            Null -> undefined
            Node a Null Null -> a
            Node _ _ b -> treeMaximum b

treeContains :: Ord a => a -> BinarySearchTree a -> Bool
treeContains = undefined -- TODO

treeFlattenOrdered :: BinarySearchTree a -> [a]
treeFlattenOrdered = undefined -- TODO

treeInsert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
treeInsert = undefined -- TODO
