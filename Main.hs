module Main where

import Geometry
import MolMec
import System.Random
import Data.List (delete)

monteCarloStep :: RandomGen g
               => MolecularSystem -> g
               -> ((Double, g), MolecularSystem)
monteCarloStep (atoms, bonds, bondAngles, bondTorsAngles) g =
    ((totalEnergy ms', g'), ms')
  where
    (r,g') = randomR (0, (length atoms)-1) g
    (r',g'') = randomVector g'
    a = atoms !! r
    a' = a { pos = (pos a) @+ r' }
    atoms' = replaceAtomInAtoms a a' atoms
    bonds' = replaceAtomInBonds a a' bonds
    bondAngles' = replaceAtomInBondAngles a a' bondAngles
    bondTorsAngles' = replaceAtomInBondTorsAngles a a' bondTorsAngles
    ms' = (atoms', bonds', bondAngles', bondTorsAngles')

monteCarlo :: RandomGen g 
           => [Atom] 
           -> [Bond] 
           -> Integer 
           -> g 
           -> (MolecularSystem, g, Double)
monteCarlo atoms bonds iterations g =
    loop (atoms,bonds,bondAngles,bondTorsAngles) g iterations
  where
    bondAngles = collectBondAngles bonds
    bondTorsAngles = collectBondTorsAngles bondAngles
    loop :: RandomGen g 
         => MolecularSystem -> g 
         -> Integer -> (MolecularSystem, g, Double)
    loop ms g 0 = (ms, g, totalEnergy ms)
    loop ms g n = if (v'-v) < 0 then loop ms' g' (n-1) else loop ms g' (n-1)
      where
        v = totalEnergy ms
        ((v', g'), ms') = monteCarloStep ms g

main :: IO ()
main = do 
    v <- return $ totalEnergy (atoms,bonds,bondAngles,bondTorsAngles)
    (ms', _, v') <- return $ monteCarlo atoms bonds 100 (mkStdGen 1234)
    putStrLn " ----- "
    putStrLn (show v)
    putStrLn (show v')
  where
    atoms = [ Atom {pos=Vector 0 0 0,charge=0,radius=1,atomId=1}
            , Atom {pos=Vector 1 0 0,charge=0,radius=1,atomId=2}
            , Atom {pos=Vector 2 2 2,charge=0,radius=1,atomId=3}
            , Atom {pos=Vector 5 5 3,charge=1,radius=1.5,atomId=4}
            ]
    bonds = [ Bond (atoms!!0) (atoms!!1)
            , Bond (atoms!!0) (atoms!!2)
            ]
    bondAngles = collectBondAngles bonds
    bondTorsAngles = collectBondTorsAngles bondAngles
