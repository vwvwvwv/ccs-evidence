module Main where

import Geometry
import MolMec
import System.Random
import Data.List (delete)

main :: IO ()
main = do
    totalV <- return $ totalEnergy atoms bonds bondAngles bondTorsAngles
    putStrLn $ show totalV
    a <- return $ atoms !! fst (randomR (0,(length atoms)-1) $ mkStdGen 111)
    a' <- return $ a { pos = (pos a) @+ fst (randomVector $ mkStdGen 1324) }
    putStrLn $ (show a) ++ "\n" ++ (show a')
    atoms' <- return $ replaceAtomInAtoms a a' atoms
    bonds' <- return $ replaceAtomInBonds a a' bonds
    bondAngles' <- return $ replaceAtomInBondAngles a a' bondAngles
    bondTorsAngles' <- return $ replaceAtomInBondTorsAngles a a' bondTorsAngles
    totalV' <- return $ totalEnergy atoms' bonds' bondAngles' bondTorsAngles'
    putStrLn $ show totalV'
    putStrLn $ show $ totalV' - totalV
  where
    atoms = [ Atom {pos=Vector 0 0 0,charge=0,radius=1,atomId=1}
            , Atom {pos=Vector 1 2 3,charge=0,radius=1,atomId=2}
            , Atom {pos=Vector 2 2 2,charge=0,radius=1,atomId=3}
            , Atom {pos=Vector 5 5 3,charge=1,radius=1.5,atomId=4}
            ]
    bonds = [ Bond (atoms!!0) (atoms!!1)
            , Bond (atoms!!0) (atoms!!2)
            ]
    bondAngles = collectBondAngles bonds
    bondTorsAngles = collectBondTorsAngles bondAngles
