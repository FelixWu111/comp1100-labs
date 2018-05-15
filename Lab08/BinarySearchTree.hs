module BinarySearchTree where

import BinaryTree

type BinarySearchTree a = BinaryTree a

treeIsValid :: Ord a => BinarySearchTree a -> Bool
treeIsValid tree = case tree of
    Null -> True
    Node _ Null Null -> True
    Node e (Node l _ _) Null -> l < e
    Node e Null (Node r _ _) -> r > e
    Node _ t1 t2 -> treeIsValid t1 && treeIsValid t2

treeMinimum :: BinarySearchTree a -> a
treeMinimum tree = case tree of
                Null -> error "There's no minimum for a Null tree"
                Node e Null _ -> e
                Node _ t1 _ -> treeMinimum t1

treeMaximum :: BinarySearchTree a -> a
treeMaximum tree = case tree of
                Null -> error "There's no maximum for a Null tree"
                Node e _ Null -> e
                Node _ _ t2 -> treeMaximum t2

treeContains :: Ord a => a -> BinarySearchTree a -> Bool
treeContains _ Null = False
treeContains x (Node v t1 t2)
    | x == v = True
    | x < v = treeContains x t1
    | x > v = treeContains x t2

treeFlattenOrdered :: BinarySearchTree a -> [a]
treeFlattenOrdered tree = case tree of
                         Null -> []
                         (Node n a b) -> treeFlattenOrdered a ++ [n] ++ treeFlattenOrdered b

treeInsert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
treeInsert x Null = Node x Null Null
treeInsert x (Node v t1 t2)
    | v == x = Node v t1 t2
    | v < x = Node v (treeInsert x t1) t2
    | v > x = Node v t1 (treeInsert x t2)
