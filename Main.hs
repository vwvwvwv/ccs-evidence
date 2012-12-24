module Main where

import Geometry
import MolMec
import MonteCarlo
import System.Random (mkStdGen)

oxygen = Atom { elementName = "O"
              , mass        = 15.999
              , charge      = 0
              , radius      = 1.52
              , pos         = Vector 0 0 0
              , atomId      = 1
              }

hydrogen = Atom { elementName = "H"
                , mass        = 1.008
                , charge      = 0
                , radius      = 1.2
                , pos         = Vector 0 0 0
                , atomId      = 1
                }

-- Our system: H2O and O2, placed more or less randomly.
atoms = [ oxygen   { pos = Vector 1.0 2.0 3.0, atomId = 0 }
        , hydrogen { pos = Vector 2.0 3.0 1.0, atomId = 1 }
        , hydrogen { pos = Vector 0.0 0.0 2.0, atomId = 2 }
        , oxygen   { pos = Vector 4.0 0.0 0.0, atomId = 3 }
        , oxygen   { pos = Vector 2.0 0.0 0.0, atomId = 4 }
        ]

bonds = [ Bond (atoms!!0) (atoms!!1)
        , Bond (atoms!!0) (atoms!!2)
        , Bond (atoms!!3) (atoms!!4)
        ]

main :: IO ()
main = do
    v <- return $ totalEnergy ms
    displayMolecularSystem ms
    putStrLn " Total energy: (before) "
    putStrLn (show v)
    putStrLn ""
    putStrLn $ " Calculating " ++ (show numberOfIterations) ++ " iterations..."
    ((v', g'), ms') <- return $ monteCarlo ms numberOfIterations (mkStdGen seed)
    putStrLn ""
    displayMolecularSystem ms'
    putStrLn " Total energy: (after) "
    putStrLn (show v')
  where
    bondAngles         = collectBondAngles bonds
    bondTorsAngles     = collectBondTorsAngles bondAngles
    ms                 = (atoms,bonds,bondAngles,bondTorsAngles)
    numberOfIterations = 10000
    seed               = 4
