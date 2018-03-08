-- COMP1100
-- Semester 1, 2018
-- Week 2 Lab
-- <Wu Yu>, February 2018
module Mensuration where
-- Here are a few simple mensuration definitions:
cube :: Integer -> Integer
cube x = x * x * x

edgeLength :: Integer
edgeLength = 3

volume :: Integer
volume = cube edgeLength

-- If I remember correctly, this is the formula for the surface area
-- of a sphere, in terms of its radius:
surfaceAreaWithRadius :: Float -> Float
surfaceAreaWithRadius r = 4.0 * pi * r^2

areaOfTriangle :: Float -> Float -> Float -> Maybe Float
areaOfTriangle a b c
 | a + b <= c || a +c <= b || c + b <= a = Nothing
 | a + b > c || a + c > b || b + c > a = Just v
 |otherwise = error "No solution"
                       where s = (a + b + c)/2
                             v = sqrt (s * (s - a) * (s - b) * (s - c))

uglyQuadraticFormula :: Float -> Float -> Float -> (Float, Float)
uglyQuadraticFormula a b c = ((-b + sqrt (b*b - 4*a*c))/ (2*a),
                             (-b - sqrt (b*b - 4*a*c))/ (2*a))

prettyQuadraticFormula :: Float -> Float -> Float -> (Float, Float)
prettyQuadraticFormula a b c = ((-b + sqrtDiscriminant) / denomintor,
                               (-b - sqrtDiscriminant) / denomintor)
                               where
                                 sqrtDiscriminant = sqrt (b*b - 4*a*c)
                                 denomintor = 2*a

data Quadrants = Origin |QuadrantI | QuadrantII | QuadrantIII | QuadrantIV |XAxisPositive | XAxisNegative | YAxisPositive | YAxisNegative
   deriving (Show, Eq)
quadrant :: Float -> Float -> Quadrants
quadrant x y
 | x > 0 && y > 0 = QuadrantI
 | x > 0 && y < 0 = QuadrantIV
 | x < 0 && y > 0 = QuadrantII
 | x < 0 && y < 0 = QuadrantIII
 | x == 0 && y > 0 = YAxisPositive
 | x == 0 && y < 0 = YAxisNegative
 | x > 0 && y == 0 = XAxisPositive
 | x < 0 && y == 0 = XAxisNegative
 | x == 0 && y == 0 = Origin

myatan2 :: Float -> Float -> Float
myatan2 y x
 | x > 0 && y >= 0 = (y / x)
 | x > 0 && y < 0 = (y / x)
 | x < 0 && y >= 0 = (y / x) + pi
 | x < 0 && y < 0 = (y / x) - pi
 | x == 0 && y > 0 = pi/2
 | x == 0 && y < 0 = -pi/2
 | x == 0 && y == 0 = undefined

