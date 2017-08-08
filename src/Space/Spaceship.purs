module Space.Spaceship where

import Prelude (map, (*), (/), (+), (-), (<<<), (<>))
import Math (cos, sin, pi)
import Data.Number.Format (fixed, toStringWith)
import Data.List (List(..), foldl, (:))
import Graphics.WebGLAll (Buffer)
import Data.ArrayBuffer.Types as T

type Ship = {
  name :: String,
  model :: Buffer T.Float32,
  x :: Number,
  y :: Number,
  r :: Number,
  dx :: Number,
  dy :: Number,
  dr :: Number
}

type Game = { ship :: Ship }

burnAccel :: Number
burnAccel = 0.005

data Rotation = Left | Right | Neutral

rotateAccel :: Number
rotateAccel = 0.01

showShip :: Ship -> String
showShip s = "Ship " <> s.name <> " " <> valuesDescr
  where
  values :: List Number
  values = s.x : s.y : s.r : s.dx : s.dy : s.dr : Nil
  format :: Number -> String
  format n = toStringWith (fixed 2) n
  valuesDescr :: String
  valuesDescr = joinStr ", " (map format values)

joinStr :: String -> List String -> String
joinStr _ Nil = ""
joinStr s (x : Nil) = x
joinStr s (x : xs) = foldl (\a b -> b <> s <>  a) x xs

velocity :: Ship -> Ship
velocity s = s { x = s.x + s.dx, y = s.y + s.dy, r = s.r + s.dr }

accelerate :: Boolean -> Ship -> Ship
accelerate true s = s { dx = s.dx + burnAccel * (cos s.r), dy = s.dy + burnAccel * (sin s.r) }
accelerate false s = s

rotateWithAccel :: Rotation -> Ship -> Ship
rotateWithAccel Left s = s { dr = s.dr - rotateAccel }
rotateWithAccel Right s = s { dr = s.dr + rotateAccel }
rotateWithAccel Neutral s = s

rotRate :: Number
rotRate = 5.0 * pi / 180.0
rotate :: Rotation -> Ship -> Ship
rotate Left s = s { r = s.r - rotRate }
rotate Right s = s { r = s.r + rotRate }
rotate Neutral s = s


type ShipControl = { turn :: Rotation, burn :: Boolean }

shipLogic :: ShipControl -> Ship -> Ship
shipLogic input = rotate input.turn <<< accelerate input.burn <<< velocity
