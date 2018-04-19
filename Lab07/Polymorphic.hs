module Polymorphic where

import Data.Complex
import Data.Ratio
import Prelude hiding (product)

stringLength :: String -> Int
stringLength x = case x of
                [] -> 0
                (x:xs) -> 1 + stringLength xs

integerListLength :: [Integer] -> Int
integerListLength x = case x of
                [] -> 0
                (x:xs) -> 1 + integerListLength xs

-- | A polymorphic length function
polymorphicLength :: [a] -> Int
polymorphicLength list = case list of
  []     -> 0
  _ : xs -> 1 + polymorphicLength xs

-- | A polymorphic reverse function
reverseOf :: [a] -> [a]
reverseOf xs = case xs of
            [] -> []
            c: cs -> reverseOf cs ++ [c]


-- | A polymorphic isPalindrome function
isPalindrome :: (Eq a) => [a]  -> Bool
isPalindrome xs
                     | null xs = True
                     | xs == reverseOf xs = True
                     | otherwise = False

-- | A polymorphic list equality function
listEqual :: (Eq a) => [a] -> [a] -> Bool
listEqual list1 list2 = case (list1, list2) of
  ([], [])       -> True
  ([], _ )       -> False
  (_ , [])       -> False
  (x: xs, y: ys) -> (x == y) && (xs `listEqual` ys)

isMonotonicallyIncreasing :: (Ord a) => [a]  -> Bool
isMonotonicallyIncreasing list = case list of
                             []         -> True
                             [x]        -> True
                             i1: i2: is | i1 <= i2  -> isMonotonicallyIncreasing (i2: is)
                                        | otherwise -> False

normaliseVector :: (Floating a) => [a] -> [a]
normaliseVector vector = divideByScalar vector (norm vector)

divideByScalar :: (Fractional a) => [a] -> a -> [a]
divideByScalar vector' scalar = case vector' of
                        []    -> []
                        f: fs -> (f / scalar): divideByScalar fs scalar

norm :: (Floating  a) => [a] -> a
norm vector' = sqrt (sumSqr vector')
     where
                 sumSqr :: (Num a) => [a] -> a
                 sumSqr vector'' = case vector'' of
                    []    -> 0
                    f: fs -> f*f + sumSqr fs

reallyPolymorphicLength :: (Integral b) => [a] -> b
reallyPolymorphicLength list = case list of
  []    -> 0
  _: xs -> 1 + reallyPolymorphicLength xs

printLargestAsString :: (Ord a, Show a) => [a] -> String
printLargestAsString list =
  "The largest element in the list is " ++ show (maximum list)

rationalZero :: Ratio Integer
floatZero    :: Float
doubleZero   :: Double
complexZero  :: Complex Double

rationalZero = 0
floatZero    = 0.0
doubleZero   = 0.0
complexZero  = mkPolar 0.0 0.0

product :: undefined -- TODO
product = undefined -- TODO
