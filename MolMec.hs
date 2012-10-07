module MolMec where

import Geometry
import Data.List (nub, delete, (\\))
import Data.Maybe (fromJust, isNothing)

data Atom = Atom 
    { pos :: Vector      -- ^ The position of the atom.
    , charge :: Double   -- ^ The electro-static charge of the atom.
    , radius :: Double   -- ^ The Van Der-Waals radius of the atom.
    , atomId :: Integer  -- ^ The unique ID of the atom.
    } deriving (Show, Eq)

-- | Allow atoms to be sorted by their ID.
instance Ord Atom where
  (<=) a a' = (atomId a) <= (atomId a')

data Bond = Bond Atom Atom
          deriving (Show, Eq)

-- | BondAngles are analogous to Angles in Geometry.hs, but are instead
-- made of atoms.
data BondAngle = BondAngle Atom Atom Atom
               deriving (Show, Eq)

-- | BondTorsAngles are analogous to TorsAngles in Geometry.hs.
data BondTorsAngle = BondTorsAngle Atom Atom Atom Atom
                   deriving (Show, Eq)

type MolecularSystem = ([Atom], [Bond], [BondAngle], [BondTorsAngle])

-- | Calculate the amount of energy made by through the Van Der-Waals effect
-- between two atoms. This causes atoms to become like ``soft-spheres'' that
-- are slightly attracted when at their radii touch and are quickly repelled
-- when they overlap.
vanDerWaals :: Atom -> Atom -> Double
vanDerWaals a a' = if r == 0 then 0 else 4*((1/r)^(12) - (1/r)^(6))
  where
    r = distance (pos a) (pos a')

-- | Calculate the amount of energy created by the stretching or contracting
-- of a bond. As in common in classical molecular mechanics, it is modeled as
-- a simple spring.
stretchEnergy :: Bond -> Double
stretchEnergy (Bond a a') = (k*(d-d0)^2)/2
  where
    k = 2.5 -- the ``stiffness'' of the bonds
    d = distance (pos a) (pos a')
    d0 = (radius a) + (radius a')

-- | Calculate the amount of energy created by the angles of bonds.
bendEnergy :: BondAngle -> Double
bendEnergy b = (k*(theta-theta0)^2)/2
  where
    k = 2.5 -- the ``stiffness'' of the bond angles
    theta = measureBondAngle b
    theta0 = 2*pi/3 -- the angle at rest

-- | Calculate the amount of energy created by the rotation of atoms along a
-- spine, such as in chains of hydrocarbons.
torsionalEnergy :: BondTorsAngle -> Double
torsionalEnergy b = torsBarrier*(1+s*(cos(n*phi)))/2
  where
    torsBarrier = 1 -- please look this up again
    s = 1 -- s = 1 or s = -1 (means it is conformational or eclipsed)
    n = 3 -- periodicity (the number of maxima per full revolution)
    phi = measureBondTorsAngle b

-- | Calculate the Coulomb effect between pairs of atoms.
electroStaticEnergy :: Atom -> Atom -> Double
electroStaticEnergy a a' = q*q'/k*r
  where
    q = charge a
    q' = charge a'
    k = 20    -- dielectric of the medium between interacting charges,
              -- approximated between vacuum [k=1] and water [k=80]
    r = distance (pos a) (pos a')

-- | Calculate the total energy of the system.
totalEnergy :: MolecularSystem -> Double
totalEnergy (as, bs, bas, btas) = stretch + bend + tors + vdw + elec
  where
    stretch = sum . map stretchEnergy $ bs
    bend = sum . map bendEnergy $ bas
    tors = sum . map torsionalEnergy $ btas
    vdw = sum [ vanDerWaals a a' | a <- as, a' <- as, not $ a > a' ]
    elec = sum [ electroStaticEnergy a a' | a <- as, a' <- as, not $ a > a' ]

-- | Convert a BondAngle into an Angle and find its measure.
measureBondAngle :: BondAngle -> Double
measureBondAngle (BondAngle a a' a'') = measureAngle (Angle v v' v'')
  where
    v = pos a
    v' = pos a'
    v'' = pos a''

-- | Convert a BondTorsAngle into a TorsAngle and find its measure.
measureBondTorsAngle :: BondTorsAngle -> Double
measureBondTorsAngle (BondTorsAngle a a' a'' a''') = 
    measureTorsAngle (TorsAngle v v' v'' v''')
  where
    v = pos a
    v' = pos a'
    v'' = pos a''
    v''' = pos a'''

-- | Try to create a BondAngle from two Bonds. In order for this to work,
-- there must be exactly one atom that is the same in both bonds. Currently,
-- this is not as safe as it could be, so try not to use it much.
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

-- | Find all of the angles made by bonds in a list of Bond. This is usually
-- done at the beginning of a simulation, and then never called again.
collectBondAngles :: [Bond] -> [BondAngle]
collectBondAngles bonds = 
    [ ba | b <- bonds, b' <- bonds, b /= b',
      let mba = formBondAngle b b', not $ isNothing mba,
      let ba@(BondAngle a _ a'') = fromJust mba,
      not $ a > a'' ]

-- | Attempt to create a torsional angle from two BondAngles. This code is
-- currently a mess, and almost certainly has a bug or two.
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

-- | Find all of the torsional angles made by angles in a list of Angle. As
-- with ``collectBondAngles'', this is usually called at the beginning and
-- then its results are used for the remainder of the time.
collectBondTorsAngles :: [BondAngle] -> [BondTorsAngle]
collectBondTorsAngles bondAngles =
    [ bta | ba <- bondAngles, ba' <- bondAngles, ba /= ba',
      let mbta = formBondTorsAngle ba ba', not $ isNothing mbta,
      let bta@(BondTorsAngle a _ _ a''') = fromJust mbta,
      not $ a > a''' ]

-- | Replace an Atom in a list of Atoms.
replaceAtomInAtoms :: Atom -> Atom -> [Atom] -> [Atom]
replaceAtomInAtoms a a' as = a':(delete a as)

-- | Replace all instances of an Atom in a list of Bonds.
replaceAtomInBonds :: Atom -> Atom -> [Bond] -> [Bond]
replaceAtomInBonds a a' bs = [Bond a' a'' | (Bond t t') <- bs, a `elem` [t,t'], 
                                let a'' = if a == t then t' else t]

-- | Replace all instances of an Atom in a list of BondAngles.
replaceAtomInBondAngles :: Atom -> Atom -> [BondAngle] -> [BondAngle]
replaceAtomInBondAngles a a' bas = [BondAngle t t' t'' | (BondAngle u u' u'') <- bas,
                                      let t = if u == a then a' else u,
                                      let t' = if u' == a then a' else u',
                                      let t'' = if u'' == a then a' else u'']

-- | Replace all instances of an Atom in a list of BondTorsAngles.
replaceAtomInBondTorsAngles :: Atom -> Atom -> [BondTorsAngle] -> [BondTorsAngle]
replaceAtomInBondTorsAngles a a' btas = 
    [BondTorsAngle t t' t'' t''' | (BondTorsAngle u u' u'' u''') <- btas,
                                   let t = if u == a then a' else u,
                                   let t' = if u' == a then a' else u',
                                   let t'' = if u'' == a then a' else u'',
                                   let t''' = if u''' == a then a' else u''']

-- | Replace all instances of an 'Atom' in a 'MolecularSystem'.
replaceAtomInMolecularSystem :: Atom -> Atom -> MolecularSystem -> MolecularSystem
replaceAtomInMolecularSystem a a' (atoms, bonds, bondAngles, bondTorsAngles) =
    (replaceAtomInAtoms a a' atoms,
    replaceAtomInBonds a a' bonds, 
    replaceAtomInBondAngles a a' bondAngles, 
    replaceAtomInBondTorsAngles a a' bondTorsAngles)
