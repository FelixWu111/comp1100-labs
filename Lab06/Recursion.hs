module Recursion where

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}
{-# ANN module ("HLint: ignore Use foldl"::String) #-}
{-# ANN module ("HLint: ignore Use list literal pattern"::String) #-}

sum1 :: [Integer] -> Integer
sum1 list =
  case list of
    []   -> 0
    x:xs -> x + sum1 xs

sum2 :: (Num a) => [a] -> a
sum2 list =
  case list of
    []       -> 0
    x:[]     -> x
    x:(y:zs) -> sum2((x+y):zs)

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

factorial1 :: Integer -> Integer
factorial1 n = accumulate n 1
  where
    accumulate :: Integer -> Integer -> Integer
    accumulate 0 a = a
    accumulate x a = accumulate (x-1) (x * a)

data Creature = Salmon | Puffin | Fox | Bear | Human
    deriving (Eq, Enum, Show)

happyCreature :: Creature -> String
happyCreature creature = case creature of
  Salmon -> "the " ++ show creature ++ " who is always happy"
  _      -> "the " ++ show creature ++ " who dreams of eating "
            ++ happyCreature (pred creature)
            ++ " which makes the " ++ show creature ++ " happy"

-- | Multiply the elements of a list
-- >>> product' [2,3,4]
-- 24
product' :: (Num a) => [a] -> a
product' = undefined -- TODO

-- | Convert a String to all upper case
-- >>> convertToUpperCase "xYzZy"
-- "XYZZY"
convertToUpperCase :: String -> String
convertToUpperCase = undefined -- TODO

-- | Reverse the elements of a list
-- >>> invert [1,2,3,4]
-- [4,3,2,1]
invert :: [a] -> [a]
invert = undefined -- TODO

-- | Return all subsequences of the list that add up to the target sum
-- >>> rucksack [3,7,5,9,13,17] 30
-- [[13,17],[3,5,9,13]]
rucksack :: [Integer] -> Integer -> [[Integer]]
rucksack = undefined -- TODO
