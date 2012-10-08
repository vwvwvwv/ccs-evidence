module MonteCarlo where

import Geometry
import MolMec
import System.Random

type SystemState = ((Double, StdGen), MolecularSystem)

monteCarloStep :: MolecularSystem -> StdGen -> SystemState
monteCarloStep ms@(atoms,_,_,_) g =
    ((totalEnergy ms', g'), ms')
  where
    (r,g')   = randomR (0, (length atoms)-1) g
    (r',g'') = randomVector g'
    a        = atoms !! r
    a'       = a { pos = (pos a) @+ r' }
    ms'      = replaceAtomInMolecularSystem a a' ms

monteCarlo :: MolecularSystem -> Integer -> StdGen -> SystemState
monteCarlo ms iterations g =
    loop ms g iterations
  where
    loop :: MolecularSystem -> StdGen -> Integer -> SystemState
    loop ms g 0 = ((totalEnergy ms, g), ms)
    loop ms g n = if (v'-v) < 0 then loop ms' g' (n-1) else loop ms g' (n-1)
      where
        v = totalEnergy ms
        ((v', g'), ms') = monteCarloStep ms g
