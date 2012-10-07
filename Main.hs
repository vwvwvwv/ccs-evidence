module Main where

import Geometry
import MolMec
import System.Random
import Data.List (delete)

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

main :: IO ()
main = do 
    v <- return $ totalEnergy ms
    ((v', g'), ms') <- return $ monteCarlo ms 10000 (mkStdGen 1234)
    putStrLn " ----- "
    putStrLn (show v)
    putStrLn (show v')
  where
    bondAngles = collectBondAngles bonds
    bondTorsAngles = collectBondTorsAngles bondAngles
    ms = (atoms,bonds,bondAngles,bondTorsAngles)

atoms = [ Atom {pos=Vector 0 0 0,charge=0,radius=1,atomId=1}
        , Atom {pos=Vector 1 0 0,charge=0,radius=1,atomId=2}
        , Atom {pos=Vector 2 2 2,charge=0,radius=1,atomId=3}
        , Atom {pos=Vector 5 5 3,charge=1,radius=1.5,atomId=4}
        ]

bonds = [ Bond (atoms!!0) (atoms!!1)
        , Bond (atoms!!0) (atoms!!2)
        ]

