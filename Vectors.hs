module Vectors where

data Vector a = Vect a a a
              deriving (Show, Eq)

foldVect1    :: (Num a) => (a -> a -> a) -> [Vector a] -> Vector a
foldVect1 op = foldr1 applyOp
  where
    applyOp (Vect x0 y0 z0) (Vect x1 y1 z1) =
      Vect (op x0 x1) (op y0 y1) (op z0 z1)

mapVect    :: (Num a) => (a -> a) -> [Vector a] -> [Vector a]
mapVect op = map applyOp
  where
    applyOp (Vect x y z) = Vect (op x) (op y) (op z)

sumVectors = foldVect1 (+)

subtractVectors :: [Vector Double] -> Vector Double
subtractVectors = foldVect1 (-)

dotVectors :: (Num a) => [Vector a] -> a
dotVectors = dot . foldVect1 (*)
  where
    dot :: (Num a) => Vector a -> a
    dot (Vect x y z) = x+y+z

scaleVectors s = mapVect (* s)

