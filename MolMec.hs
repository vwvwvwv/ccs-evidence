import Geometry
import Data.List

data Atom = Atom { pos :: Vector
                 , charge :: Double
                 , radius :: Double
                 }
          deriving (Show, Eq)

data Bond = Bond Atom Atom

data BondAngle = BondAngle Atom Atom Atom

data BondTorsAngle = BondTorsAngle Atom Atom Atom Atom

vanDerWaals :: Atom -> Atom -> Double
vanDerWaals a a' = 4*(r^(-12) - r^(-6))
  where
    r = distance (pos a) (pos a')

stretchEnergy :: Bond -> Double
stretchEnergy (Bond a a') = (k*(d-d0)^2)/2
  where
    k = 2.5 -- the ``stiffness'' of the bonds
    d = distance (pos a) (pos a')
    d0 = (radius a) + (radius a')

bendEnergy :: BondAngle -> Double
bendEnergy b = (k*(theta-theta0)^2)/2
  where
    k = 2.5 -- the ``stiffness'' of the bond angles
    theta = measureBondAngle b
    theta0 = 2*pi/3 -- the angle at rest

torsionalEnergy :: BondTorsAngle -> Double
torsionalEnergy b = torsBarrier*(1+s*(cos(n*phi)))/2
  where
    torsBarrier = 1 -- please look this up again
    s = 1 -- s = 1 or s = -1 (means it is conformational or eclipsed)
    n = 3 -- periodicity (the number of maxima per full revolution)
    phi = measureBondTorsAngle b

electroStaticEnergy :: Atom -> Atom -> Double
electroStaticEnergy a a' = q*q'/k*r
  where
    q = charge a
    q' = charge a'
    k = 20    -- dielectric of the medium between interacting charges,
              -- approximated between vacuum [k=1] and water [k=80]
    r = distance (pos a) (pos a')

measureBondAngle :: BondAngle -> Double
measureBondAngle (BondAngle a a' a'') = measureAngle (Angle v v' v'')
  where
    v = pos a
    v' = pos a'
    v'' = pos a''

measureBondTorsAngle :: BondTorsAngle -> Double
measureBondTorsAngle (BondTorsAngle a a' a'' a''') = 
    measureTorsAngle (TorsAngle v v' v'' v''')
  where
    v = pos a
    v' = pos a'
    v'' = pos a''
    v''' = pos a'''
