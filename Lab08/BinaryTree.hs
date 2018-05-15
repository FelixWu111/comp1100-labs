module BinaryTree (
      BinaryTree (Null, Node),
      treeSize,     -- :: Integral b => BinaryTree a -> b
      treeDepth,    -- :: Integral b => BinaryTree a -> b
      treeFlatten,  -- :: BinaryTree a -> [a]
      treeLeaves,   -- :: BinaryTree a -> [a]
      treeMap       -- :: (a -> b) -> BinaryTree a -> BinaryTree b
) where

data List a
  = Empty
  | Cons a
         (List a)
  deriving (Show, Eq)

data BinaryTree a
  = Null
  | Node a
         (BinaryTree a)
         (BinaryTree a)
  deriving (Show, Eq)

tree1 :: BinaryTree Int
tree1 =
  Node
    5
    (Node
       4
       (Node 2 Null (Node 11 Null Null))
       (Node 1 (Node 0 Null Null) (Node (-3) Null Null)))
    (Node 3 (Node 8 (Node (-4) Null Null) (Node 7 Null Null)) Null)

-- Exercise 1
treeSize :: Integral b => BinaryTree a -> b
treeSize tree = case tree of
    Null -> 0
    (Node _ a b) -> 1 + treeSize a + treeSize b

treeDepth :: Integral b => BinaryTree a -> b
treeDepth tree = case tree of
    Null -> 0
    (Node _ a b) -> 1 + max (treeDepth a) (treeDepth b)

treeFlatten :: BinaryTree a -> [a]
treeFlatten tree = case tree of
    Null -> []
    (Node n a b) -> treeFlatten a ++ [n] ++ treeFlatten b

treeLeaves :: BinaryTree a -> [a]
treeLeaves tree = case tree of
    Null -> []
    (Node a Null Null) -> [a]
    (Node _ b Null) -> treeLeaves b
    (Node _ b c) -> treeLeaves b ++ treeLeaves c

-- Exercise 2
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap func tree = case tree of
    Null -> Null
    (Node e t1 t2) -> (Node (func e) (treeMap func t1) (treeMap func t2))
