import CodeWorld

tree :: Integer -> Double -> Picture
tree 0 f = colored yellow (solidCircle (f*pi/20))
tree n f =
  polyline [(0,0),(0,1)] &
  translated 0 1
    (rotated ( pi/10) (tree (n-1) f) &
     rotated (-pi/10) (tree (n-1) f))

main :: IO ()
main = animationOf (tree 8 . atan)
