module Boom where

boom :: Integer -> Integer -> Integer
boom m n
  | m <= 0 = n + 1
  | n <= 0 = boom (m-1) 1
  | otherwise = boom (m-1) (boom m (n-1))