module Main where

import Geometry
import MolMec
import MonteCarlo
import System.Random (mkStdGen)

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

