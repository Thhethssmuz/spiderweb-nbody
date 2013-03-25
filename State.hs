module State 
  ( Body (..)
  , makeBody
  , ForceV
  , State (..) 
  , makeState
  , gravity
  ) where

import Vector
import Data.IORef ( IORef, newIORef )
import Graphics.UI.GLUT ( Vertex2, GLfloat )
import Data.Time

-------------------------------------------------------------------------------
-- Constants 
-------------------------------------------------------------------------------
gravity :: Double
gravity = 1.0

-------------------------------------------------------------------------------
-- Body
-------------------------------------------------------------------------------
data Body = Body {
  position         :: IORef (Vector Double) ,
  velocity         :: IORef (Vector Double) ,
  acceleration     :: IORef (Vector Double) ,
  mass             :: Double ,
  radius           :: Double }

makeBody :: [Double] -> [Double] -> Double -> Double -> IO Body
makeBody p v m r = do
  pRef <- newIORef $ toVector p
  vRef <- newIORef $ toVector v
  aRef <- newIORef $ toVector [0,0]

  return Body {
    position         = pRef ,
    velocity         = vRef ,
    acceleration     = aRef ,
    mass             = m ,
    radius           = r }

-------------------------------------------------------------------------------
-- Entire State

type ForceV = (Vertex2 GLfloat, Vertex2 GLfloat, GLfloat)

data State = State {
  debugText  :: IORef String ,
  lastUpdate :: IORef UTCTime ,
  nbody      :: IORef [Body] ,
  forcesV    :: IORef [ForceV] }


makeState :: IO State
makeState = do
  debugTextRef   <- newIORef $ "debug"

  lastUpdateTime <- getCurrentTime
  lastUpdateRef  <- newIORef $ lastUpdateTime

  -- Make some test bodies to put into the nbody state
  b101 <- makeBody [-400,0] [0,0] (2^5) 5
  b102 <- makeBody [400,0] [0,0] (2^5) 5
  b103 <- makeBody [-40,0] [0,-10] (2^5) 5
  b104 <- makeBody [40,0] [0,10] (2^5) 5
  b105 <- makeBody [0,10] [0,0] (2^5) 5
  b106 <- makeBody [0,-10] [0,0] (2^5) 5

  b1  <- makeBody [-400,0] [0,0] (2^5) 5
  b2  <- makeBody [400,0] [0,0] (2^5) 5
  b3  <- makeBody [-40,0] [0,0] (2^5) 5
  b4  <- makeBody [40,0] [0,0] (2^5) 5
  b5  <- makeBody [0,10] [0,0] (2^5) 5
  b6  <- makeBody [0,-10] [0,0] (2^5) 5
  b7  <- makeBody [13,12] [0,0] (2^5) 5
  b8  <- makeBody [-21,50] [0,0] (2^5) 5
  b9  <- makeBody [38,801] [0,0] (2^5) 5
  b10 <- makeBody [82,-50] [0,0] (2^5) 5
  b11 <- makeBody [84,20] [0,0] (2^5) 5
  b12 <- makeBody [86,110] [0,0] (2^5) 5
  b13 <- makeBody [-87,-20] [0,0] (2^5) 5
  b14 <- makeBody [-48,12] [0,0] (2^5) 5
  b15 <- makeBody [59,24] [0,0] (2^5) 5
  b16 <- makeBody [61,-158] [0,0] (2^5) 5
  b17 <- makeBody [10,36] [0,0] (2^5) 5
  b18 <- makeBody [-23,121] [0,0] (2^5) 5
  b19 <- makeBody [-32,-42] [0,0] (2^5) 5
  b20 <- makeBody [-44,53] [0,0] (2^5) 5
  b21 <- makeBody [55,64] [0,0] (2^5) 5
  b22 <- makeBody [66,78] [0,0] (2^5) 5
  b23 <- makeBody [711,84] [0,0] (2^5) 5
  b24 <- makeBody [87,95] [0,0] (2^5) 5
  b25 <- makeBody [95,-16] [0,0] (2^5) 5
  b26 <- makeBody [14,54] [0,0] (2^5) 5
  b27 <- makeBody [-29,15] [0,0] (2^5) 5
  b28 <- makeBody [-32,-56] [0,0] (2^5) 5
  b29 <- makeBody [-43,11] [0,0] (2^5) 5
  b30 <- makeBody [511,22] [0,0] (2^5) 5
  b31 <- makeBody [65,63] [0,0] (2^5) 5
  b32 <- makeBody [74,-41] [0,0] (2^5) 5
  b33 <- makeBody [-86,20] [0,0] (2^5) 5
  b34 <- makeBody [198,-60] [0,0] (2^5) 5
  b35 <- makeBody [19,-40] [0,0] (2^5) 5
  b36 <- makeBody [11,80] [0,0] (2^5) 5
  b37 <- makeBody [-23,45] [0,0] (2^5) 5
  b38 <- makeBody [-54,-24] [0,0] (2^5) 5
  b39 <- makeBody [-45,38] [0,0] (2^5) 5
  b40 <- makeBody [74,45] [0,0] (2^5) 5

  nbodiesRef <- newIORef $ []
{-
  nbodiesRef <- newIORef $ [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15
                           ,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28
                           ,b29,b30,b31,b32,b33,b34,b35,b36,b37,b38,b39,b40]
-}

  forcesVRef <- newIORef $ []
  return State {
    debugText  = debugTextRef ,
    lastUpdate = lastUpdateRef ,
    nbody      = nbodiesRef ,
    forcesV    = forcesVRef }






main = do
  putStrLn "It compiles!"