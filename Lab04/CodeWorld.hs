{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame

spread :: Integer -> Picture -> Double -> Picture
spread 0 _ _ = blank
spread n pic dx = pic & translated dx 0 (spread (n-1) pic dx)

ourPicture :: Picture
ourPicture = spread 3 (trafficLight True) 4

main :: IO ()
main = drawingOf ourPicture