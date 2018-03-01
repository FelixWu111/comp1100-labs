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

areaOfTriangle :: Float -> Float -> Float -> Float
areaOfTriangle a b c = sqrt (s * (s - a) * (s - b) * (s - c))
                       where s = (a + b + c)/2

uglyQuadraticFormula :: Float -> Float -> Float -> (Float, Float)
uglyQuadraticFormula a b c = ((-b + sqrt (b*b - 4*a*c))/ (2*a),
                             (-b - sqrt (b*b - 4*a*c))/ (2*a))

prettyQuadraticFormula :: Float -> Float -> Float -> (Float, Float)
prettyQuadraticFormula a b c = ((-b + sqrtDiscriminant) / denomintor,
                               (-b - sqrtDiscriminant) / denomintor)
                               where
                                 sqrtDiscriminant = sqrt (b*b - 4*a*c)
                                 denomintor = 2*a