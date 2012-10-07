module MolMec where

import Geometry
import Data.List (nub, delete, (\\))
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

type MolecularSystem = ([Atom], [Bond], [BondAngle], [BondTorsAngle])

vanDerWaals :: Atom -> Atom -> Double
vanDerWaals a a' = if r == 0 then 0 else 4*((1/r)^(12) - (1/r)^(6))
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
    if null same then Nothing
      else let (a':[]) = same
               (a:a'':[]) = as' \\ [a']
           in Just $ BondAngle a a' a''
  where
    as = [u,u',v,v']
    as' = nub as
    same = as \\ as'

collectBondAngles :: [Bond] -> [BondAngle]
collectBondAngles bonds = 
    [ ba | b <- bonds, b' <- bonds, b /= b',
      let mba = formBondAngle b b', not $ isNothing mba,
      let ba@(BondAngle a _ a'') = fromJust mba,
      not $ a > a'' ]

-- not entirely sure if this function works, yet
formBondTorsAngle :: BondAngle -> BondAngle -> Maybe BondTorsAngle
formBondTorsAngle b@(BondAngle u u' u'') b'@(BondAngle v v' v'') =
    if null same then Nothing
      else let (t:t':[]) = same
               (a:a''':[]) = as' \\ [t,t']
           in -- this monstrosity
             if (t == u && t == v) then Just $ BondTorsAngle u'' u'  v'  v''
               else if (t == u'' && t == v'') then Just $ BondTorsAngle u'  u'' v'' v'
                 else if (t == u   && t == v'') then Just $ BondTorsAngle u'' u'  v'' v'
                   else if (t == u'' && t == v) then Just $ BondTorsAngle u'  u'' v'  v''
                     else error "Something is terribly wrong in formBondTorsAngle"
  where
    as = [u,u',u'',v,v',v'']
    as' = nub as
    same = as \\ as'

collectBondTorsAngles :: [BondAngle] -> [BondTorsAngle]
collectBondTorsAngles bondAngles =
    [ bta | ba <- bondAngles, ba' <- bondAngles, ba /= ba',
      let mbta = formBondTorsAngle ba ba', not $ isNothing mbta,
      let bta@(BondTorsAngle a _ _ a''') = fromJust mbta,
      not $ a > a''' ]

totalEnergy :: MolecularSystem -> Double
totalEnergy (as, bs, bas, btas) = stretch + bend + tors + vdw + elec
  where
    stretch = sum . map stretchEnergy $ bs
    bend = sum . map bendEnergy $ bas
    tors = sum . map torsionalEnergy $ btas
    vdw = sum [ vanDerWaals a a' | a <- as, a' <- as, not $ a > a' ]
    elec = sum [ electroStaticEnergy a a' | a <- as, a' <- as, not $ a > a' ]

replaceAtomInAtoms :: Atom -> Atom -> [Atom] -> [Atom]
replaceAtomInAtoms a a' as = a':(delete a as)

replaceAtomInBonds :: Atom -> Atom -> [Bond] -> [Bond]
replaceAtomInBonds a a' bs = [Bond a' a'' | (Bond t t') <- bs, a `elem` [t,t'], 
                                let a'' = if a == t then t' else t]

replaceAtomInBondAngles :: Atom -> Atom -> [BondAngle] -> [BondAngle]
replaceAtomInBondAngles a a' bas = [BondAngle t t' t'' | (BondAngle u u' u'') <- bas,
                                      let t = if u == a then a' else u,
                                      let t' = if u' == a then a' else u',
                                      let t'' = if u'' == a then a' else u'']

replaceAtomInBondTorsAngles :: Atom -> Atom -> [BondTorsAngle] -> [BondTorsAngle]
replaceAtomInBondTorsAngles a a' btas = 
    [BondTorsAngle t t' t'' t''' | (BondTorsAngle u u' u'' u''') <- btas,
                                   let t = if u == a then a' else u,
                                   let t' = if u' == a then a' else u',
                                   let t'' = if u'' == a then a' else u'',
                                   let t''' = if u''' == a then a' else u''']

replaceAtomInMolecularSystem :: Atom -> Atom -> MolecularSystem -> MolecularSystem
replaceAtomInMolecularSystem a a' (atoms, bonds, bondAngles, bondTorsAngles) =
    (replaceAtomInAtoms a a' atoms,
    replaceAtomInBonds a a' bonds, 
    replaceAtomInBondAngles a a' bondAngles, 
    replaceAtomInBondTorsAngles a a' bondTorsAngles)
