module MolMec where

import Vectors

data Atom = Atom { name :: String
                 , pos :: Vector Double
                 , vel :: Vector Double
                 , accel :: Vector Double
                 , mass :: Double
                 , radius :: Double
                 , charge :: Double
                 }
            deriving (Show, Eq)

data Bond = Bond { length :: Double
                 , strength :: Double
                 , atom1 :: Atom
                 , atom2 :: Atom
                 } 
            deriving Show

distance a b = sqrt $ (dotVectors [v, v] :: Double)
  where
    v = subtractVectors [a, b]

lennardJones   :: Double -> Double
lennardJones r = 4 * ((1/r)^12 - (1/r)^6)

elecV         :: Atom -> Atom -> Double -> Double
elecV a1 a2 k = if rij == 0 then 0 else qi * qj / k * rij
  where
    qi = charge a1
    qj = charge a2
    rij = distance (pos a1) (pos a2)

getTotalEnergy atoms = elec + lj
  where
    cartAtoms = [(x,y) | x <- atoms, y <- atoms, x /= y]
    elec = sum $ map (\(x, y) -> elecV x y 1.0) cartAtoms
    lj = sum (map
              (\(x, y) -> lennardJones
                          (distance (pos x) (pos y)))
             cartAtoms)
  
test1 = Atom { name = "Hydrogen"
             , pos = Vect 0 0 0
             , vel = Vect 0 0 0
             , accel = Vect 0 0 0
             , mass = 1
             , radius = 1
             , charge = 1
             }
  
test2 = Atom { name = "Hydrogen"
             , pos = Vect 1 1 1
             , vel = Vect 0 0 0
             , accel = Vect 0 0 0
             , mass = 1
             , radius = 1
             , charge = (-1)
             }

-- getTotalEnergy [test1,test2]
