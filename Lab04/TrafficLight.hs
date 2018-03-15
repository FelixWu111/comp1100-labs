import CodeWorld

botCircle, topCircle, midCircle:: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0   0  (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

data Number = A | B | C deriving (Show)
trafficLight :: Number -> Picture
trafficLight A  = botCircle green & midCircle black & topCircle black & frame
trafficLight B   = botCircle black & midCircle yellow & topCircle black & frame
trafficLight C  = botCircle black & midCircle black & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
  | (round (t/2) :: Int) `mod` 5 <= 1  = trafficLight A
  | (round (t/2) :: Int) `mod` 5 == 2  = trafficLight B
  | otherwise = trafficLight C

main :: IO()
main = animationOf trafficController
