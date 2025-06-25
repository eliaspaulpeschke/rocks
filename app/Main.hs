{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, disableCursor, isKeyPressed, isKeyDown, enableCursor, getKeyPressed)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid,  drawLine3D)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3, Camera2D (Camera2D), pattern Vector2, Rectangle (Rectangle), KeyboardKey (KeyUp, KeyDown, KeyLeftControl, KeyRightControl, KeyM, KeyLeft, KeyR, KeyRight), Color)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow, mode2D)
import Raylib.Util.Colors (orange, white, black)
import Linear (V3(V3), V2(V2))
import Raylib.Util.Camera (cameraMove)
import Raylib.Core.Textures (colorAlpha)
import Raylib.Core.Shapes (drawTriangleLines)
import Raylib.Util.Math (vector2Rotate)

w :: Float
w = 1400

h :: Float
h = 800

donutClamp :: Float -> Float -> Float -> Float
donutClamp v lower upper
    | v < lower = upper
    | v > upper = lower
    | otherwise = v

data AppState = AppState {
      camera2D :: Camera2D
    , shipPos :: V2 Float
    , shipRot :: Float
    , shipVel :: V2 Float 
    , shipRotVel :: Float
    }

initialAppState :: AppState
initialAppState = AppState {
      camera2D = Camera2D (V2 (w/2) (h/2)) (V2 (w/2) (h/2)) 0 1.0 
    , shipPos = V2 (w/2) (h/2) 
    , shipRot = 0
    , shipVel = V2 0 0 
    , shipRotVel = 0
}

keyboardVal :: KeyboardKey -> a -> a -> IO a
keyboardVal k up down = do
    d <- isKeyDown k 
    pure $ if d then down else up 

drawShip :: V2 Float -> Float -> IO ()
drawShip pos rot = do
    drawTriangleLines tip right left white
    drawTriangleLines tip2 right2 left2 white
    where
    mk x = pos + vector2Rotate x rot 
    tip = mk $ V2 0 25
    right = mk $ V2 12 (-7)
    left = mk $ V2 (-12) (-7)
    tip2 = mk $ V2 0 15
    right2 = mk $ V2 18 (-5)
    left2 = mk $ V2 (-18) (-5) 




main :: IO ()
main = do
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
                pos = shipPos appstate
                rot = shipRot appstate
                vel = shipVel appstate
                rotVel = shipRotVel appstate
              in do
              drawing
                ( do
                    clearBackground black 
                    mode2D cam2D 
                       ( do
                           drawShip pos rot
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
                            . (\(a, b) -> (vector2Rotate (a * 0.5) newRot
                                          , b * 0.01)) )
                            defaultK v_
              let p = (\(V2 x y) -> V2 (donutClamp x 0 w) (donutClamp y 0 h)) $ pos + vel
              pure appstate {shipRot = newRot, shipPos = p, shipVel = vel + fst v, shipRotVel = rotVel + snd v }
          )
          initialAppState 
    )

