module Geometry where

import System.Random

-- | The Vector data type which this library is based around.
data Vector = Vector Double Double Double
            deriving (Show, Eq)

-- | The Angle data type. The middle vector is always the angle's point.
data Angle = Angle Vector Vector Vector
            deriving (Show, Eq)

-- | The TorsAngle data type for torsional angles, which are essentially two
-- angles that share an edge.
data TorsAngle = TorsAngle Vector Vector Vector Vector
            deriving (Show, Eq)

vector0 :: Vector
vector0 = Vector 0 0 0

-- | Returns a random vector with each of its indicies in the half-open
-- interval [0, 1)
randomVector :: RandomGen g => g -> (Vector, g)
randomVector g = (Vector r r' r'', g''')
  where
    (r, g') = random g
    (r', g'') = random g'
    (r'', g''') = random g''

-- | Create a function that works on doubles to work on vectors
wrapVector :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
wrapVector f (Vector x x' x'') (Vector y y' y'') = 
    Vector (f x y) (f x' y') (f x'' y'')

-- | Add two vectors.
(@+) :: Vector -> Vector -> Vector 
(@+) = wrapVector (+)

-- | Subtract two vectors.
(@-) :: Vector -> Vector -> Vector
(@-) = wrapVector (-)

-- | Simple multiplication of two vectors.
(@*) :: Vector -> Vector -> Vector 
(@*) = wrapVector (*)

-- | Simple division of two vectors.
(@/) :: Vector -> Vector -> Vector
(@/) = wrapVector (/)

-- | Dot product of two vectors.
(@.) :: Vector -> Vector -> Double
(@.) (Vector x x' x'') (Vector y y' y'') = x*y + x'*y' + x''*y''

-- | Cross product of two vectors.
(@#) :: Vector -> Vector -> Vector
(@#) (Vector x x' x'') (Vector y y' y'') = 
    Vector (x'*y''-x''*y') (x''*y-x*y'') (x*y'-x'*y)

-- | Calculate the distance between two vectors acting as points.
distance :: Vector -> Vector -> Double
distance v v' = sqrt $ v @. v'

-- | Find the length of a vector.
magnitude :: Vector -> Double
magnitude v = distance v v

-- | Calculate the measure of the angle made by three vectors acting as points.
measureAngle :: Angle -> Double
measureAngle (Angle v v' v'') =
    asin $ (magnitude (a @# b)) / ((magnitude a) * (magnitude b))
  where
    a = v @- v'
    b = v'' @- v'

-- | Calculate the measure of the torsional angle made by four vectors acting
-- as points.
measureTorsAngle :: TorsAngle -> Double
measureTorsAngle (TorsAngle v v' v'' v''') = 
    asin $ (magnitude (a @# b)) / ((magnitude a) * (magnitude b))
  where
    a = (v'@-v) @# (v''@-v)
    b = (v'@-v''') @# (v''@-v''')
