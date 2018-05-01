module HigherOrder where

import Prelude hiding (product, sum)
import Data.Char

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
applyFunction func integerValue = func integerValue

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
applyFunctionOverList fuc x = map fuc x


-- | selectWhereTrue
-- Examples:
--
--  >>> selectWhereTrue isNegative [0.0, 1.0, -1.0, -9.2, 3.0]
-- [-1.0, -9.2]
--
-- >>> selectWhereTrue isPositive [0.0, 1.0, -1.0, -9.2, 3.0]
-- [1.0, 3.0]
selectWhereTrue :: (Double -> Bool) -> [Double] -> [Double]
selectWhereTrue fuc x = case x of
                  [] -> []
                  (x:xs)
                        |fuc x  -> x: selectWhereTrue fuc xs
                        |not(fuc x) -> selectWhereTrue fuc xs

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
applyFunctionOverList' :: (a -> b) -> [a] -> [b]
applyFunctionOverList' f x = case x of
                           [] -> []
                           (x:xs) -> f x : applyFunctionOverList' f xs

-- | selectWhereTrue'
selectWhereTrue' :: (a -> Bool) -> [a] -> [a]
selectWhereTrue' f x = case x of
                 [] -> []
                 (x:xs)
                    | f x -> x:selectWhereTrue' f xs
                    |not(f x) -> selectWhereTrue' f xs

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
combineListsWithBinaryOperation :: (a->a->a) -> [a] -> [a] -> [a]
combineListsWithBinaryOperation f  a b = case a of
                               [] -> case b of
                                           [] -> []
                                           (y:ys)-> []
                               (x:xs) -> case b of
                                           [] -> []
                                           (y:ys) -> f x y : combineListsWithBinaryOperation f xs ys

-- | combineElementsIntoTuples
combineElementsIntoTuples :: [a] -> [b] -> [(a, b)]
combineElementsIntoTuples [] [] = []
combineElementsIntoTuples [] (x:xs) = []
combineElementsIntoTuples (x:xs) [] = []
combineElementsIntoTuples (x:xs) (y:ys) = (x,y):combineElementsIntoTuples xs ys

-- | combineElementsIntoTuples'
combineElementsIntoTuples':: (a->a->a) -> [a] -> [a] -> [a]
combineElementsIntoTuples' f a b = combineListsWithBinaryOperation f a b


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
foldRight f e xs = case xs of
                [] -> e
                (x:xs) -> foldRight f (f x e) xs

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
foldLeft f e xs = case xs of
                   [] -> e
                   (x:xs) -> foldLeft f (f e x) xs

-- | Reimplement sum using foldLeft or foldRight

sum :: Num a => [a] -> a
sum list = foldRight (+) 0 list


-- | Reimplement product using foldLeft or foldRight

product :: Num a => [a] -> a
product list = foldRight (*) 1 list

-- | Reimplement allTrue using foldLeft or foldRight

allTrue :: [Bool] -> Bool
allTrue list = foldRight (&&) True list


-- | Reimplement anyTrue using foldLeft or foldRight

anyTrue :: [Bool] -> Bool
anyTrue list = foldRight (&&)  False list


-- | convertToLower
convertToLower :: String -> String
convertToLower list = map toLower list

-- | removeNonAlphanum
removeNonAlphanum :: String -> String
removeNonAlphanum list = filter isAlphaNum list

-- | dotProduct
dotProduct :: [Int]->[Int]->[Int]
dotProduct a b = combineListsWithBinaryOperation (*)  a b
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
-- Example:
--
-- >>> sumOfSquaresUpTo 1000
-- 10416
sumOfSquaresUpTo :: Int -> Int
sumOfSquaresUpTo n = foldLeft (+) 0 (filter isSquare [1 .. n])

-- | largestSquareSmallerThan
-- Example:
--
-- >>> largestSquareSmallerThan 1000
-- 961
largestSquareSmallerThan :: Int -> Int
largestSquareSmallerThan n = last (filter isSquare [1 .. n])

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
isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

-- | primeFactors
primeFactors :: Integer -> [Integer]
primeFactors n = case factors of
                [] -> [n]
                _  -> factors ++ primeFactors (n `div` (head factors))
                where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

