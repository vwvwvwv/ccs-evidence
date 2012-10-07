module Geometry where

import System.Random

data Vector = Vector Double Double Double
            deriving (Show, Eq)

data Angle = Angle Vector Vector Vector
            deriving (Show, Eq)

data TorsAngle = TorsAngle Vector Vector Vector Vector
            deriving (Show, Eq)

vector0 :: Vector
vector0 = Vector 0 0 0

randomVector :: RandomGen g => g -> (Vector, g)
randomVector g = (Vector r r' r'', g''')
  where
    (r, g') = random g
    (r', g'') = random g'
    (r'', g''') = random g''

wrapVector :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
wrapVector f (Vector x x' x'') (Vector y y' y'') = 
    Vector (f x y) (f x' y') (f x'' y'')

(@+) :: Vector -> Vector -> Vector 
(@+) = wrapVector (+)

(@-) :: Vector -> Vector -> Vector
(@-) = wrapVector (-)

(@*) :: Vector -> Vector -> Vector 
(@*) = wrapVector (*)

(@/) :: Vector -> Vector -> Vector
(@/) = wrapVector (/)

(@.) :: Vector -> Vector -> Double
(@.) (Vector x x' x'') (Vector y y' y'') = x*y + x'*y' + x''*y''

(@#) :: Vector -> Vector -> Vector
(@#) (Vector x x' x'') (Vector y y' y'') = 
    Vector (x'*y''-x''*y') (x''*y-x*y'') (x*y'-x'*y)

distance :: Vector -> Vector -> Double
distance v v' = sqrt $ v @. v'

magnitude :: Vector -> Double
magnitude v = distance v v

measureAngle :: Angle -> Double
measureAngle (Angle v v' v'') =
    asin $ (magnitude (a @# b)) / ((magnitude a) * (magnitude b))
  where
    a = v @- v'
    b = v'' @- v'

measureTorsAngle :: TorsAngle -> Double
measureTorsAngle (TorsAngle v v' v'' v''') = 
    asin $ (magnitude (a @# b)) / ((magnitude a) * (magnitude b))
  where
    a = (v'@-v) @# (v''@-v)
    b = (v'@-v''') @# (v''@-v''')
