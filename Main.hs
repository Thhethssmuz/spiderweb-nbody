
import State
import Vector
import MonadExpansion
import Control.Monad
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT hiding ( position )
import Data.Time

import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL as GL hiding ( position )
-------------------------------------------------------------------------------
-- Timer / update
-------------------------------------------------------------------------------
timer :: State -> TimerCallback
timer state = do

  -- Calculate Delta Time
  t0 <- get $ lastUpdate state
  t1 <- getCurrentTime
  lastUpdate state $= t1
  let dt = realToFrac $ diffUTCTime t1 t0

  -- Update and move
  bs <- get $ nbody state

{-  Parallel variant
  bsP <- uniqueCombineM (\b1 b2 -> return (b1,b2)) bs
  bs' <- uniqueCombineM (\b1 b2 -> do p1 <- get $ position b1
                                      p2 <- get $ position b2
                                      return (p1,mass b1,p2, mass b2)
                                      ) bs
  let bsr = (map calcAccel' bs') `using` parListChunk 1016 rdeepseq
  mapM_ (\b -> acceleration b $= toVector [0,0]) bs
  zipWithM_ (\(b1,b2) (a1,a2) -> do acceleration b1 $~ (+) a1
                                    acceleration b2 $~ (+) a2
                                    ) bsP bsr
-}

{-  Pre-Parallelism
-}
  mapM_ (\b -> acceleration b $= toVector [0,0]) bs
  fs <- uniqueCombineM calcAccel bs

  mapM_ (move dt) bs

  -- put force vectors into state for drawing
  forcesV state $= fs

  -- Draw all the things!
  postRedisplay Nothing

  -- calculate time till next update 
  let u = if   d < 1
          then 0
          else if d > 15
               then 16
               else d
        where 
          d = 16 + round (16 - dt * 1000)
  addTimerCallback u (timer state)

  -- Debug
  debugText state $= "FPS: " ++ show (round (1/dt))


move :: Double -> Body -> IO ()
move dt b = do
  a <- get $ acceleration b
  velocity b $~ (+) (a * (realToFrac dt))
  v <- get $ velocity b
  position b $~ (+) (v * (realToFrac dt))

calcAccel' :: (Vector Double, Double, Vector Double, Double) -> (Vector Double, Vector Double)
calcAccel' (p1,m1,p2,m2) =
  let r   = p2-p1
      d2  = magnitude2 r
      eps = 1000
      ff  = realToFrac (1e5 * m1 * m2) / realToFrac ((d2 + eps^2)**(3/2))
      f   = r * realToFrac ff
  in (f / realToFrac m1, -f / realToFrac m2)

calcAccel :: Body -> Body -> IO (ForceV)
calcAccel b1 b2 = do
  p1 <- get $ position b1
  p2 <- get $ position b2

  let m1  = mass b1
      m2  = mass b2
      r   = p2-p1
      d2  = magnitude2 r
      eps = 1000
      ff  = realToFrac (1e5 * m1 * m2) / realToFrac ((d2 + eps^2)**(3/2))
      f   = r * realToFrac ff

  acceleration b1 $~ (+) (f / realToFrac m1)
  acceleration b2 $~ (+) (-f / realToFrac m2)

  let (x1:y1:_) = toList p1
      (x2:y2:_) = toList p2
  return $ ( Vertex2 (realToFrac x1) (realToFrac y1)
           , Vertex2 (realToFrac x2) (realToFrac y2)
           , ff )

errorTimer :: State -> TimerCallback
errorTimer state = do
  bs <- get $ nbody state
  k <- mapM (\b -> do v <- get $ velocity b; return $ v) bs
  debugText state $= "error: " ++ show (magnitude . sum $ k)
  addTimerCallback 1000 (errorTimer state) -- 16 ~ 60fps, 33 ~ 30 fps

-------------------------------------------------------------------------------
-- Keyboard bindings
-------------------------------------------------------------------------------
keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state (MouseButton b) Down _ (Position x y) = do
  case b of 
    LeftButton -> do (Size w h) <- get windowSize
                     b <- makeBody [fromIntegral x - fromIntegral w/2, 
                                    fromIntegral h/2 - fromIntegral y] 
                                   [0,0] (2^5) 5
                     nbody state $~ (++) [b]
    _          -> return ()

  return ()

keyboardMouse _ _ _ _ _ = return ()

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------
display :: State -> DisplayCallback
display state = do
  clear [ ColorBuffer ]
  loadIdentity

  -- Draw all nbodies
  bs <- get $ nbody state
  color $ Color4 1 1 1 (0.5 :: GLfloat)
  mapM_ drawBody bs

  -- Draw all forces
  fs <- get $ forcesV state
  drawString (-895) 390 (show . length $ fs)

  renderPrimitive Lines $ do
    mapM_ (\(x,y,a) -> do color $ Color4 0 1 0 (a :: GLfloat)
                          vertex x
                          vertex y) fs


  -- Draw (0,0) mark
  color $ Color4 1 1 1 (1 :: GLfloat)
  renderPrimitive Points $ do
    vertex $ Vertex2 0 (0 :: GLfloat)

  -- Debug
  debug <- get $ debugText state
  drawString (-895) 430 debug
  drawString (-895) 410 ("Particles: " ++ show (length bs))

  swapBuffers

-------------------------------------------------------------------------------
-- Draw Helper functions
-------------------------------------------------------------------------------
drawString :: GLfloat -> GLfloat -> String -> IO ()
drawString x y s = do
  currentRasterPosition $= Vertex4 x y 0 1
  renderString Fixed9By15 s

drawBody :: Body -> IO ()
drawBody b = do
  p <- get $ position b
  let (x:y:_) = toList p
      r       = radius b
  drawCircle (realToFrac x) (realToFrac y) (realToFrac r)

drawCircle :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawCircle x y r = do
  renderPrimitive LineLoop $ do 
    mapM_ c [0..15]
    where
      c k = vertex $ Vertex2 (x + r * cos (2*pi*k/16)) (y + r * sin (2*pi*k/16))

-------------------------------------------------------------------------------
-- Main ++
-------------------------------------------------------------------------------
reshape :: ReshapeCallback
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size) 
  matrixMode $= Projection
  loadIdentity
  ortho (fromIntegral w/(-2)) (fromIntegral w/2) 
        (fromIntegral h/(-2)) (fromIntegral h/2) (-1) 0
  matrixMode $= Modelview 0


main :: IO ()
main = do
  getArgsAndInitialize
  initialWindowSize $= Size 1800 890
  initialWindowPosition $= Position 60 75
  initialDisplayMode $= [ DoubleBuffered, RGBAMode ]
  createWindow "Testitest"

  blend $= Enabled
  blendFunc $= (GL.SrcAlpha, OneMinusSrcAlpha)
--  shadeModel $= Flat


  state <- makeState
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse state)
  addTimerCallback 100 (timer state)
--  addTimerCallback 1000 (errorTimer state)

  mainLoop


