import Geometry
import Data.List (nub, (\\))
import Data.Maybe (fromJust, isNothing)

data Atom = Atom { pos :: Vector
                 , charge :: Double
                 , radius :: Double
                 , atomId :: Integer
                 }
          deriving (Show, Eq)

instance Ord Atom where
  (<=) a a' = (atomId a) <= (atomId a')

data Bond = Bond Atom Atom
          deriving (Show, Eq)

data BondAngle = BondAngle Atom Atom Atom
               deriving (Show, Eq)

data BondTorsAngle = BondTorsAngle Atom Atom Atom Atom
                   deriving (Show, Eq)

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

formBondAngle :: Bond -> Bond -> Maybe BondAngle
formBondAngle b@(Bond u u') b'@(Bond v v') =
    if null same
      then Nothing 
      else let (a':[]) = same
               (a:a'':[]) = as' \\ [a']
           in Just $ BondAngle a a' a''
  where
    as = [u,u',v,v']
    as' = nub as
    same = as' \\ as

collectBondAngles :: [Bond] -> [BondAngle]
collectBondAngles bonds = 
    [ ba | b@(Bond u u') <- bonds, b'@(Bond v v') <- bonds, b /= b',
      let mba = formBondAngle b b', not $ isNothing mba,
      let ba@(BondAngle a _ a'') = fromJust mba,
      not $ a > a'' ]

totalEnergy :: [Atom] -> [Bond] -> [BondAngle] -> [BondTorsAngle] -> Double
totalEnergy as bs bas btas = stretch + bend + tors + vdw + elec
  where
    stretch = sum . map stretchEnergy $ bs
    bend = sum . map bendEnergy $ bas
    tors = 0 -- must implement collectBondTorsAngles first
    vdw = sum [ vanDerWaals a a' | a <- as, a' <- as, not $ a > a' ]
    elec = sum [ electroStaticEnergy a a' | a <- as, a' <- as, not $ a > a' ]
