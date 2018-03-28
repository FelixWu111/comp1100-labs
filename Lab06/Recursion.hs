module Recursion where

import Prelude hiding (product)
import Data.Char

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}
{-# ANN module ("HLint: ignore Use foldl"::String) #-}
{-# ANN module ("HLint: ignore Use list literal pattern"::String) #-}

-- | sum1 : Compute the sum of the elements of a list of Integer
-- Use a straightforward primtive recursion to compute the sum,
-- with one case dealing with empty lists, and the other with non-empty lists.
--
-- Examples:
-- >>> sum1 []
-- 0
-- >>> sum1 [1,2,3,4]
-- 10
-- >>> sum1 [-1,1]
-- 0
sum1 :: [Integer] -> Integer
sum1 x = case x of
         [] -> 0
         [x] -> x
         (x:xs)-> x + sum1 xs


-- | sum2 : Compute the sum of the elements of a list of Integer
-- This time, use three cases:
-- one case for the empty list, returning 0,
-- a second case for a list of at least one element, returning x,
-- and a third case for a list of at least two elements x and y, plus a tail zs,
-- returning the result of invoking sum2 recursively on the (one shorter) list
-- constructed with (x+y) as head and zs as tail.
--
-- prop> sum1 l == sum2 l
--
sum2 :: (Num a) => [a] -> a
sum2 x =case x of
             [] -> 0
             [x] -> x
             x:y:xs-> (x + y) + sum2 xs

-- | sum3 :
sum3 :: (Num a) => [a] -> a
sum3 list = accumulate 0 list
  where
    accumulate a []     = a
    accumulate a (x:xs) = accumulate (a + x) xs

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
factorial _ = error "factorial undefined for negative integers"

forgotBaseCase :: Integer -> Integer
forgotBaseCase n = n * forgotBaseCase (n-1)

steppingOnTheSpot :: Integer -> Integer
steppingOnTheSpot 0 = 1
steppingOnTheSpot n | n > 0 = n * steppingOnTheSpot n
steppingOnTheSpot _ = error "undefined for negative integers"

steppingTheWrongDirection :: Integer -> Integer
steppingTheWrongDirection 0 = 1
steppingTheWrongDirection n | n > 0 = n * steppingTheWrongDirection (n+1)
steppingTheWrongDirection _ = error "undefined for negative integers"

factorial2 :: Integer -> Integer
factorial2 n = accumulate 1 n
  where
    accumulate :: Integer -> Integer -> Integer
    accumulate 1 x
                     |x==1 = 1
                     |x > 1 = x * accumulate 1 (x-1)

data Creature = Salmon | Puffin | Fox | Bear | Human
    deriving (Eq, Enum, Show)

happyCreature :: Creature -> String
happyCreature creature = case creature of
  Salmon -> "the " ++ show creature ++ " who is always happy"
  _      -> "the " ++ show creature ++ " who dreams of eating "
            ++ happyCreature (pred creature)
            ++ " which makes the " ++ show creature ++ " happy"

-- | Multiply the elements of a list
-- >>> product [2,3,4]
-- 24
product :: (Num a) => [a] -> a
product x =case x of
             []-> 1
             [x]-> x
             (x:xs) -> x * product xs

-- | Convert a String to all upper case
-- >>> convertToUpperCase "xYzZy"
-- "XYZZY"
convertToUpperCase :: String -> String
convertToUpperCase x = case x of
                      ""-> ""
                      (x:xs) -> toUpper x : convertToUpperCase xs


-- | Reverse the elements of a list
-- >>> invert [1,2,3,4]
-- [4,3,2,1]
invert :: [a] -> [a]
invert x = case x of
              [] -> []
              (x:xs)-> invert xs ++ [x]

-- | Return all subsequences of the list that add up to the target sum
-- >>> rucksack [3,7,5,9,13,17] 30
-- [[13,17],[3,5,9,13]]
rucksack :: [Integer] -> Integer -> [[Integer]]
rucksack (x:xs) y =undefined -- TODO

