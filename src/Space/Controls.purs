module Space.Controls where

import Signal.DOM (keyPressed)
import Prelude (bind, pure, (<$>), (<*>))
import Space.Spaceship
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Signal (Signal)

leftKeyCode :: Int
leftKeyCode = 37

rightKeyCode :: Int
rightKeyCode = 39

burnKeyCode :: Int
burnKeyCode = 38

inputToRotation :: Boolean -> Boolean -> Rotation
inputToRotation true false = Left
inputToRotation false true = Right
inputToRotation _ _ = Neutral

inputToControl :: Boolean -> Boolean -> Boolean -> ShipControl
inputToControl left right burn = {
  turn: inputToRotation left right,
  burn: burn
}

controls :: forall eff. Eff (dom :: DOM | eff) (Signal ShipControl)
controls = do
  leftInputs <- keyPressed leftKeyCode
  rightInputs <- keyPressed rightKeyCode
  burnInputs <- keyPressed burnKeyCode
  let signal = inputToControl <$> leftInputs <*> rightInputs <*> burnInputs
  pure signal
