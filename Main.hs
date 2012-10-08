module Main where

import Geometry
import MolMec
import MonteCarlo
import System.Random (mkStdGen)

main :: IO ()
main = do 
    v <- return $ totalEnergy ms
    ((v', g'), ms') <- return $ monteCarlo ms 10000 (mkStdGen 4)
    displayMolecularSystem ms
    putStrLn " ----- "
    putStrLn (show v)
    putStrLn ""
    displayMolecularSystem ms'
    putStrLn " ----- "
    putStrLn (show v')
  where
    bondAngles = collectBondAngles bonds
    bondTorsAngles = collectBondTorsAngles bondAngles
    ms = (atoms,bonds,bondAngles,bondTorsAngles)

atoms = [ Atom {elementName="A",pos=Vector 0 0 0,charge=0,radius=1,atomId=1}
        , Atom {elementName="A",pos=Vector 1 0 0,charge=0,radius=1,atomId=2}
        , Atom {elementName="A",pos=Vector 2 2 2,charge=0,radius=1,atomId=3}
        , Atom {elementName="B",pos=Vector 5 5 3,charge=1,radius=1.5,atomId=4}
        ]

bonds = [ Bond (atoms!!0) (atoms!!1)
        , Bond (atoms!!0) (atoms!!2)
        ]

