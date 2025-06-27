{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, disableCursor, isKeyPressed, isKeyDown, enableCursor, getKeyPressed, loadRandomSequence, setRandomSeed, getRandomValue)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid,  drawLine3D)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3, Camera2D (Camera2D), pattern Vector2, Rectangle (Rectangle), KeyboardKey (KeyUp, KeyDown, KeyLeftControl, KeyRightControl, KeyM, KeyLeft, KeyR, KeyRight), Color)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow, mode2D)
import Raylib.Util.Colors (orange, white, black)
import Linear (V3(V3), V2(V2))
import Raylib.Util.Camera (cameraMove)
import Raylib.Core.Textures (colorAlpha)
import Raylib.Core.Shapes (drawTriangleLines, drawLineV)
import Raylib.Util.Math (vector2Rotate)
import Data.List.Split (divvy)
import Control.Monad (foldM)

w :: Float
w = 1400

h :: Float
h = 800

donutClamp :: Float -> Float -> Float -> Float
donutClamp lower upper v
    | v < lower = upper
    | v > upper = lower
    | otherwise = v

data Object = Object {
      objPos :: V2 Float
    , objRot :: Float
    , objVel :: V2 Float
    , objRotVel :: Float
}

data Rock = Rock {
      rockData :: Object
    , rockVerts :: [(Float, Float)] 
}

data AppState = AppState {
      camera2D :: Camera2D
    , shipData :: Object
    , rockList :: [Rock]
}

initialAppState :: AppState
initialAppState = AppState {
      camera2D = Camera2D (V2 (w/2) (h/2)) (V2 (w/2) (h/2)) 0 1.0 
    , shipData = Object 
        { objPos = V2 (w/2) (h/2) 
        , objRot = -1
        , objVel = V2 0 0 
        , objRotVel = 0 }
    , rockList = [ ]
}

keyboardVal :: KeyboardKey -> a -> a -> IO a
keyboardVal k up down = do
    d <- isKeyDown k 
    pure $ if d then down else up 

drawShip :: Object -> IO ()
drawShip dat = do
    drawTriangleLines tip right left white
    drawTriangleLines tip2 right2 left2 white
    where
    mk x = objPos dat + vector2Rotate x (objRot dat)
    tip = mk $ V2 0 25
    right = mk $ V2 12 (-7)
    left = mk $ V2 (-12) (-7)
    tip2 = mk $ V2 0 15
    right2 = mk $ V2 18 (-5)
    left2 = mk $ V2 (-18) (-5) 

randomRock :: Object -> [Rock] -> IO Rock
randomRock shipObj otherRocks = do
    x <- fromIntegral <$> iterateRandom (round w) forbiddenW 60
    y <- fromIntegral <$> iterateRandom (round h) forbiddenH 60
    rot <- fromIntegral <$> getRandomValue (-100) 100
    vx <- fromIntegral <$> getRandomValue (-100) 100
    vy <- fromIntegral <$> getRandomValue (-100) 100
    let rockObj = Object {objPos=V2 x y, objRot=0, objVel=V2 (vx * 0.002) (vy* 0.002), objRotVel=rot * 0.0002}
    makeRock rockObj
    where
    objList = shipObj : map rockData otherRocks
    forbiddenW = map (\(Object {objPos = (V2 w' _)}) -> round w') objList 
    forbiddenH = map (\(Object {objPos = (V2 _ h')}) -> round h') objList
    iterateRandom target forbidden dist = do
        x <- getRandomValue 0 target
        let test = foldr (\el res -> res || abs (x - el) <= dist) False forbidden
        if not test then pure x else iterateRandom target forbidden dist

updateRock :: Rock -> Rock
updateRock rock = rock {rockData = dat {objRot = objRot dat + objRotVel dat,
                                        objPos = pos}}
    where
    dat = rockData rock
    pos = (\(V2 x y) -> V2 (donutClamp 0 w x) (donutClamp 0 h y)) (objPos dat + objVel dat)


makeRock :: Object -> IO Rock
makeRock dat = do
    verts <- mapM (\_ -> getRandomValue 5 10) ([0..11]::[Integer])
    pure Rock { rockData = dat
        , rockVerts = snd (foldr (\v (x, l) -> (x + (pi/6), (fromIntegral v * 5, x):l)) (0, []) verts)
        }

drawRock :: Rock -> IO ()
drawRock rock = do
    case length verts of
        l | l < 3 -> pure () 
        _ -> mapM_ line $ [head verts, last verts] : divvy 2 1 verts
    pure ()
    where
    verts = rockVerts rock
    obj = rockData rock
    mk (v, x) = objPos obj + vector2Rotate (vector2Rotate (V2 0 v) x) (objRot obj)
    line (a:b:_) = drawLineV (mk a) (mk b) white
    line _ = pure ()

main :: IO ()
main = do
  setRandomSeed 15
  rocks <- foldM
    (\r _ -> do
       x <- randomRock (shipData initialAppState) r 
       pure (x : r))
    []
    [0..8]
  withWindow
    (floor w) 
    (floor h)
    "test"
    60
    ( \window -> do
        whileWindowOpen_
          ( \appstate ->
              let 
                cam2D = camera2D appstate 
                shipdata = shipData appstate
                pos = objPos shipdata
                rot = objRot shipdata
                vel = objVel shipdata
                rotVel = objRotVel shipdata
                rlist = rockList appstate
              in do
              drawing
                ( do
                    clearBackground black 
                    mode2D cam2D 
                       ( do
                           drawShip shipdata
                           mapM_ drawRock rlist 
                           pure ()
                       ) 
                )
              let defaultK = (V2 0 0, 0)
              v_ <- mapM (\(k,d,u) -> keyboardVal k d u) [ 
                          (KeyUp, defaultK, (V2 0 1, 0)) 
                        , (KeyDown, defaultK, (V2 0 (-1), 0)) 
                        , (KeyLeft, defaultK, (V2 0.01 0, -1)) 
                        , (KeyRight, defaultK, (V2 (-0.01) 0, 1)) 
                       ]
              let newRot = rot + rotVel
              let v = foldr ((\(x, y) (x', y') -> (x + x', y + y')) 
                            . (\(a, b) -> (vector2Rotate (a * 0.2) newRot
                                          , b * 0.002)) )
                            defaultK v_
              let p = (\(V2 x y) -> V2 (donutClamp 0 w x) (donutClamp 0 h y)) $ pos + vel
              pure appstate { 
                    shipData = shipdata {
                     objRot = newRot, objPos = p, objVel = vel + fst v, objRotVel = rotVel + snd v }
                  , rockList = map updateRock rlist
                }
          )
          initialAppState {rockList = rocks}
    )

