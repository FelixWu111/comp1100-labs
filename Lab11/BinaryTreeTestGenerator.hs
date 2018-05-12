module BinaryTreeTestGenerator where

import BinaryTree
import Test.QuickCheck

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = oneof [pure Null, Node <$> arbitrary <*> arbitrary <*> arbitrary]

expandTreeAtFringe :: a -> BinaryTree a -> BinaryTree a
expandTreeAtFringe e tree =
  case tree of
    Null -> Node e Null Null
    Node e' Null right -> Node e' (expandTreeAtFringe e Null) right
    Node e' left Null -> Node e' left (expandTreeAtFringe e Null)
    Node e' left right -> Node e' (expandTreeAtFringe e left) right
