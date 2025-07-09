{-# LANGUAGE PatternSynonyms #-}


module Main where

import Raylib.Core (clearBackground, disableCursor, isKeyPressed, isKeyDown, enableCursor, getKeyPressed, loadRandomSequence, setRandomSeed, getRandomValue)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid,  drawLine3D)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3, Camera2D (Camera2D), pattern Vector2, Rectangle (Rectangle, rectangle'y, rectangle'x, rectangle'width, rectangle'height), KeyboardKey (KeyUp, KeyDown, KeyLeftControl, KeyRightControl, KeyM, KeyLeft, KeyR, KeyRight), Color)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow, mode2D)
import Raylib.Util.Colors (orange, white, black, red, green, yellow, blue)
import Linear (V3(V3), V2(V2), Metric (dot), R1 (_x), R2 (_y))
import Control.Lens
import Raylib.Util.Camera (cameraMove)
import Raylib.Core.Textures (colorAlpha)
import Raylib.Core.Shapes (drawTriangleLines, drawLineV, checkCollisionCircles, checkCollisionRecs, checkCollisionCircleRec, drawRectangleRec)
import Raylib.Util.Math (vector2Rotate, Vector (magnitude, vectorNormalize), normalize, vector2Angle, vector2Reflect)
import Data.List.Split (divvy)
import Control.Monad (foldM)
import qualified Debug.Trace as T

w :: Float
w = 1400

h :: Float
h = 800

donutClamp :: Ord a => a -> a -> a -> a 
donutClamp lower upper v
    | v < lower = upper
    | v > upper = lower
    | otherwise = v

donutWrap :: (Num a, Ord a) => a -> a -> a -> a
donutWrap lower upper v
    | v < lower = donutWrap lower upper $ upper - (lower - v)
    | v > upper = donutWrap lower upper $ lower + (v - upper)
    | otherwise = v

data CollisionType = CollisionCircle (V2 Float) Float | CollisionRect Rectangle | CollisionCustom CollisionType (CollisionType -> Bool)-- | CollisionTri (V2 Float) (V2 Float) (V2 Float) 

class Collision a where
    collisionType :: a -> CollisionType

collide :: (Collision a, Collision b) => a -> b -> Bool
collide a b = innerCollide (collisionType a) (collisionType b)
    where
    innerCollide :: CollisionType -> CollisionType -> Bool
    innerCollide (CollisionCircle p1 r1) (CollisionCircle p2 r2) = checkCollisionCircles p1 r1 p2 r2
    innerCollide (CollisionRect r1) (CollisionRect r2) = checkCollisionRecs r1 r2
    innerCollide (CollisionRect r1) (CollisionCircle p2 r2) = checkCollisionCircleRec p2 r2 r1
    innerCollide (CollisionCircle p1 r1) (CollisionRect r2) = checkCollisionCircleRec p1 r1 r2
    innerCollide (CollisionCustom t1 fn1) (CollisionCustom t2 fn2) = fn1 t2 && fn2 t1
    innerCollide (CollisionCustom _ fn) x = fn x
    innerCollide x (CollisionCustom _ fn) = fn x

class HasObject a where
    getObject :: a -> Object
    setObject :: a -> Object -> a

collideReflect :: (Collision a, HasObject a, Collision b, HasObject b) => a -> b -> (a, b)
collideReflect a b = if not (collide a b) then (a, b) else (a', b')
    where
    aO = getObject a
    bO = getObject b
    p = vector2Rotate (pi / 2) $ (\(V2 x y) -> atan2 y x) (objPos aO - objPos bO)
    testA = dot (objVel aO) (objPos aO - objPos bO) < 0
    testB = dot (objVel bO) (objPos aO - objPos bO) < 0
    aV = objVel aO 
    bV = objVel bO
    a' = if testA then a else setObject a $ aO { objVel = fmap (magnitude aV *) (vectorNormalize $ vector2Reflect aV p) }
    b' = if testB then b else setObject b $ bO { objVel = fmap (magnitude bV *) (vectorNormalize $ vector2Reflect bV p) }

data Object = Object {
      objPos :: V2 Float
    , objRot :: Float
    , objVel :: V2 Float
    , objRotVel :: Float
    , objMass :: Float 
}

instance HasObject Object where
    getObject = id
    setObject _ a = a

data Rock = Rock {
      rockData :: Object
    , rockVerts :: [(Float, Float)] 
}

instance HasObject Rock where
    getObject = rockData 
    setObject r o = r { rockData = o }

instance Collision Rock where
    collisionType r = CollisionCircle (objPos $ rockData r) 50

data Ship = Ship {
      shipData :: Object
    , shipSize :: V2 Float
}

instance HasObject Ship where
    getObject = shipData
    setObject s o = s { shipData = o }

instance Collision Ship where
    collisionType s = CollisionRect (Rectangle {
              rectangle'y=py - (0.5 * height)
            , rectangle'x=px - (0.5 * width)
            , rectangle'width=width
            , rectangle'height=height })
        where
        (V2 x y) = shipSize s
        r = objRot $ shipData s
        (V2 px py) = objPos $ shipData s
        width = abs (x * cos r) + abs (y * sin r)
        height = abs (x * sin r) + abs (y * cos r)


data AppState = AppState {
      camera2D :: Camera2D
    , ship :: Ship
    , rockList :: [Rock]
}

piClampRotation :: Float -> Float
piClampRotation a
    | a > pi = piClampRotation (- (pi - a))
    | a < (-pi) = piClampRotation ((-pi) - a)
    | otherwise = a

initialAppState :: AppState
initialAppState = AppState {
      camera2D = Camera2D (V2 (w/2) (h/2)) (V2 (w/2) (h/2)) 0 1.0 
    , ship = Ship { 
          shipData = Object { 
              objPos = V2 (w/2) (h/2) 
            , objRot = 0
            , objVel = V2 0 0 
            , objRotVel = 0
            , objMass = 1.0}
        , shipSize = V2 20 30 }
    , rockList = [ ]
}

keyboardVal :: KeyboardKey -> a -> a -> IO a
keyboardVal k up down = do
    d <- isKeyDown k 
    pure $ if d then down else up 

drawShip :: Ship -> IO ()
drawShip s = do
    drawTriangleLines tip right left white
    drawTriangleLines tip2 right2 left2 green 
    where
    (V2 x y) = shipSize s
    dat = shipData s
    mk x = objPos dat + vector2Rotate x (objRot dat)
    tip = mk $ V2 0 (y * 0.5)
    right = mk $ V2 (x * 0.2) (-y * 0.5) 
    left = mk $ V2 (-x * 0.2) (-y * 0.5) 
    tip2 = mk $ V2 0 (y * 0.4)
    right2 = mk $ V2 (x * 0.5) (-y * 0.3)
    left2 = mk $ V2 (-x * 0.5) (-y * 0.3)

randomRock :: Object -> [Rock] -> IO Rock
randomRock shipObj otherRocks = do
    x <- fromIntegral <$> iterateRandom (round w) forbiddenW 60
    y <- fromIntegral <$> iterateRandom (round h) forbiddenH 60
    rot <- fromIntegral <$> getRandomValue (-100) 100
    vx <- fromIntegral <$> getRandomValue (-100) 100
    vy <- fromIntegral <$> getRandomValue (-100) 100
    let rockObj = Object {objPos=V2 x y, objRot=0, objVel=V2 (vx * 0.002) (vy* 0.002), objRotVel=rot * 0.0002, objMass = 25.0}
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
        , rockVerts = snd (foldr (\v (x, l) -> (x + (pi/6), (fromIntegral v * 5, x):l)) (-pi, []) verts)
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

collideRock :: (Rock, Object) -> (Rock, Object)
collideRock (r, s) = if mag > 50 || mag > dirSize then (r, s)
    else (r', s') 
    where
    mag = magnitude (sP - rP)
    sP = objPos s
    rP = objPos $ rockData r
    dir = (\(V2 x y) -> atan2 y x) (sP - rP)
    indexDir = dir - (objRot $ rockData r)
    index = let r = (round (indexDir / (pi / 6)) + 6) in donutWrap 0 11 r
    dirSize = fst $ rockVerts r !! index
    p = vector2Rotate (pi / 2) dir
    rV = objVel $ rockData r
    sV = objVel s
    rM = magnitude rV
    sM = magnitude sV
    s' = s { objVel = fmap (sM *) (vectorNormalize $ vector2Reflect sV p) }
    r' = r { rockData = (rockData r) { objVel = fmap (rM *) (vectorNormalize $ vector2Reflect rV p )}}

main :: IO ()
main = do
  setRandomSeed 15
  rocks <- foldM
    (\r _ -> do
       x <- randomRock (getObject $ ship initialAppState) r 
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
                stShip = ship appstate
                shipdata = getObject stShip
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
                           drawShip stShip
                           let vecs = map (\n -> ((V2 (-100) 0), (V2 (100 * cos (pi * n/10)) (100 * sin (pi * n/10))))) [2, 5, 8]
                               norm = V2 0 100
                               start = V2 400 400
                           drawLineV start (start + norm) red
                           mapM_ (\(x, y) -> do drawLineV (start + x) (start + y) white
                                                drawLineV (start + V2 (x ^._x + 200) (x ^._y)) (start + y) yellow ) vecs
                           mapM_ drawRock rlist 
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

              let newstate = appstate { 
                    ship = setObject stShip shipdata {
                     objRot = newRot, objPos = p, objVel = vel + fst v, objRotVel = rotVel + snd v }
                  , rockList = map updateRock rlist
                }

              let (rlist', ship') = foldr (\r (rs, s) -> let (r',s') = collideReflect r s in (r' : rs, s')) ([], ship newstate) (rockList newstate) 

              pure newstate {ship = ship', rockList = rlist'}
          )
          initialAppState {rockList = rocks}
    )

