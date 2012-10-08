module Viewer where

import Geometry
import MolMec
import MonteCarlo
import Main
import Graphics.UI.GLUT
import Data.IORef

vectorToVertex3 :: Vector -> Vertex3 Double
vectorToVertex3 (Vector x x' x'') = Vertex3 x x' x''

viewerMain :: IO ()
viewerMain = do
    (progname, _) <- getArgsAndInitialize
    -- initialDisplayMode $= [DoubleBuffered]
    createWindow "Hello World"
    reshapeCallback $= Just reshape
    angle <- newIORef (0.0 :: GLfloat, 0.0)
    scalar <- newIORef (1.0 :: GLfloat)
    position <- newIORef (0.0 :: GLfloat, 0.0)
    keyboardMouseCallback $= Just (keyboardMouse angle position scalar)
    idleCallback $= Just idle
    displayCallback $= (display angle position scalar)
    mainLoop

addAtom :: Atom -> IO ()
addAtom a = vertex $ vectorToVertex3 $ pos a

addBond :: Bond -> IO ()
addBond (Bond a a') = do
    addAtom a
    addAtom a'
    return ()

display angle position scalar = do
    clear [ ColorBuffer ]
    loadIdentity
    (x,y) <- get position
    translate $ Vector3 x y 0
    s <- get scalar
    scale (0.3*s) (0.3*s) (0.3*s :: GLfloat)
    preservingMatrix $ do
      (theta, phi) <- get angle
      rotate theta $ Vector3 0 0 (1::GLfloat)
      rotate phi $ Vector3 (1::GLfloat) 0 0
      renderPrimitive Points $ mapM_ addAtom atoms
      renderPrimitive Lines $ mapM_ addBond bonds
    -- swapBuffers
    flush

reshape s@(Size w h) = do
    viewport $= (Position 0 0, s)

idle = do
    postRedisplay Nothing

keyboardMouse angle pos scalar key state modifiers position = do
    handler angle pos scalar key state
  where
    handler a p s (Char ' ') Down = do
      (theta, phi) <- get a
      a $= (-theta, -phi)
    handler a p s (Char 'l') Down = do
      (theta, phi) <- get a
      a $= (theta-1, phi)
    handler a p s (Char 'h') Down = do
      (theta, phi) <- get a
      a $= (theta+1, phi)
    handler a p s (Char 'k') Down = do
      (theta, phi) <- get a
      a $= (theta, phi+1)
    handler a p s (Char 'j') Down = do
      (theta, phi) <- get a
      a $= (theta, phi-1)
    handler a p s (SpecialKey KeyLeft) Down = do
      (x,y) <- get p
      p $= (x-0.05,y)
    handler a p s (SpecialKey KeyRight) Down = do
      (x,y) <- get p
      p $= (x+0.05,y)
    handler a p s (SpecialKey KeyUp) Down = do
      (x,y) <- get p
      p $= (x,y+0.05)
    handler a p s (SpecialKey KeyDown) Down = do
      (x,y) <- get p
      p $= (x,y-0.05)
    handler a p s (Char '-') Down = do
      s' <- get s
      s $= s'/2
    handler a p s (Char '+') Down = do
      s' <- get s
      s $= s'*2
    handler _ _ _ _ _ = return ()
