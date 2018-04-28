module HigherOrder where

import Prelude hiding (sum,product)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

-- | isEqual
-- Examples:
--
-- >>> isEqual 5 5.9999
-- False
--
-- >>> isEqual 6 5.9999
-- True
isEqual :: Integer -> Double -> Bool
isEqual m x = abs (fromIntegral m - x) < tolerance
  where
    tolerance = 0.0001

-- | flipArguments
-- Examples:
--
-- >>> flipArguments isEqual 5.9999 6
-- True
flipArguments :: (Integer -> Double -> Bool) -> Double -> Integer -> Bool
flipArguments func doubleValue integerValue = func integerValue doubleValue

-- | applyFunction
applyFunction :: (Integer -> Integer) -> Integer -> Integer
applyFunction = undefined -- TODO

double :: Integer -> Integer
double m = 2 * m

triple :: Integer -> Integer
triple m = m + m + m

reverseSign :: Integer -> Integer
reverseSign m = -m

-- | applyFunctionOverList
-- Examples:
--
-- >>> applyFunctionOverList double [1,2,3]
-- [2,4,6]
--
-- >>> applyFunctionOverList reverseSign [1,2,3]
-- [-1,-2,-3]
applyFunctionOverList :: (Integer -> Integer) -> [Integer] -> [Integer]
applyFunctionOverList = undefined -- TODO

-- | selectWhereTrue
-- Examples:
--
--  >>> selectWhereTrue isNegative [0.0, 1.0, -1.0, -9.2, 3.0]
-- [-1.0, -9.2]
--
-- >>> selectWhereTrue isPositive [0.0, 1.0, -1.0, -9.2, 3.0]
-- [1.0, 3.0]
selectWhereTrue = undefined -- TODO

isNegative :: Double -> Bool
isNegative x = x < 0.0

isPositive :: Double -> Bool
isPositive x = x > 0.0

-- | polymorphicLength
polymorphicLength :: Integral b => [a] -> b
polymorphicLength list =
  case list of
    [] -> 0
    _:xs -> 1 + polymorphicLength xs

-- | applyFunction'
applyFunction' :: (a -> Integer) -> a -> Integer
applyFunction' f m = f m

-- | applyFunction''
applyFunction'' :: (a -> b) -> a -> b
applyFunction'' f x = f x

-- | applyFunctionOverList'
applyFunctionOverList' = undefined -- TODO

-- | selectWhereTrue'
selectWhereTrue' = undefined -- TODO

-- | combineListsWithBinaryOperation
-- Examples:
--
-- >>> combineListsWithBinaryOperation (+) [1.0, 2.0, 3.0] [4.0, 5.0, 6.0]
-- [5.0, 7.0, 9.0]
--
-- >>> combineListsWithBinaryOperation (++) ["the", "brown", "jumps", "the"] ["quick", "fox", "over", "lazy", "dog"]
-- ["thequick", "brownfox", "jumpsover", "thelazy"]
--
-- >>> combineListsWithBinaryOperation div [1..10] [-10..0]
-- [-1, -1, -1, -1, -1, -2, -2, -3, -5, -10]
combineListsWithBinaryOperation = undefined -- TODO

-- | combineElementsIntoTuples
combineElementsIntoTuples :: [a] -> [b] -> [(a, b)]
combineElementsIntoTuples = undefined -- TODO

-- | combineElementsIntoTuples'
combineElementsIntoTuples' = undefined -- TODO

-- | foldRight
-- Examples:
--
-- >>> foldRight (+) 0 [1,2,3,4,5]
-- 15
--
-- >>> foldRight (*) 1 [1,2,3,4,5]
-- 120
--
-- >>> foldRight (-) 0 [1,2,3,4,5]
-- 3
foldRight :: (a -> b -> b) -> b -> [a] -> b -- predefined as foldr
foldRight operation identity list = undefined -- TODO

-- | foldLeft
-- Examples:
--
-- >>> foldLeft (+) 0 [1,2,3,4,5]
-- 15
--
-- >>> foldLeft (-) 0 [1,2,3,4,5]
-- -15
--
-- prop> foldLeft (+) 0 xs == foldRight (+) 0 xs
--
-- The following property does not hold:
-- prop> foldLeft (-) 0 xs == foldRight (-) 0 xs
foldLeft :: (b -> a -> b) -> b -> [a] -> b -- predefined as foldl
foldLeft operation identity list = undefined -- TODO

-- | Reimplement sum using foldLeft or foldRight
-- TODO
sum :: Num a => [a] -> a
sum list =
  case list of
    [] -> identity
    x:xs -> x + sum xs
      where identity = 0

-- | Reimplement product using foldLeft or foldRight
-- TODO
product :: Num a => [a] -> a
product list =
  case list of
    [] -> identity
    x:xs -> x * product xs
      where identity = 1

-- | Reimplement allTrue using foldLeft or foldRight
-- TODO
allTrue :: [Bool] -> Bool
allTrue list =
  case list of
    [] -> identity
    b:bs -> b && allTrue bs
      where identity = True

-- | Reimplement anyTrue using foldLeft or foldRight
-- TODO
anyTrue :: [Bool] -> Bool
anyTrue list =
  case list of
    [] -> identity
    b:bs -> b || anyTrue bs
      where identity = False

-- | convertToLower
convertToLower = undefined -- TODO

-- | removeNonAlphanum
removeNonAlphanum = undefined -- TODO

-- | dotProduct
dotProduct = undefined -- TODO

-- | isSquare
-- Examples:
--
-- >>> isSquare (10000 :: Int)
-- True
--
-- >>> isSquare (10000 :: Integer)
-- True
isSquare :: Integral a => a -> Bool
isSquare i = floor (sqrt (fromIntegral i) :: Float) ^ (2 :: Int) == i

-- | sumOfSquaresUpTo
sumOfSquaresUpTo = undefined -- TODO

-- | isPrime
-- Examples:
--
-- >>> isPrime 2
-- True
--
-- >>> isPrime 3
-- True
--
-- >>> isPrime 4
-- False
--
-- >>> isPrime 5
-- True
--
-- >>> isPrime 6
-- False
--
-- >>> isPrime 7
-- True
isPrime :: Integer -> Bool
isPrime = undefined -- TODO

-- | primeFactors
primeFactors :: Integer -> [Integer]
primeFactors = undefined -- TODO
