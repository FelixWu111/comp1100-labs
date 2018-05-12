module BinarySearchTreeTestGenerator where

import BinarySearchTree
import BinaryTree

import Data.List ()
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (BinaryTree a) where
  arbitrary = sized genTree
    where
      genTree n = fmap (foldr treeInsert Null) (vector n)
