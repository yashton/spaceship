module Space.Main (main) where

import Space.Render (ProgramBindings, render, shaders, shipMesh)
import Space.Controls (controls)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.WebGL (WebGl)
import DOM (DOM)
import Graphics.WebGLAll (WebGLContext, Capacity(DEPTH_TEST), clearColor, enable, makeBufferFloat, runWebGL, withShaders)
import Prelude (Unit, bind, discard, pure, (<$>))
import Signal (foldp, runSignal, sampleOn)
import Signal.DOM (animationFrame)
import Space.Spaceship (Game, ShipControl, shipLogic)
import Control.Monad.Eff.Console (CONSOLE, log)

initialState :: forall eff. Eff (webgl :: WebGl | eff) Game
initialState = do
    model <- makeBufferFloat shipMesh
    pure { ship: {
      name: "Player 1",
      x: 0.0, y: 0.0, r: 0.0, dx: 0.0, dy: 0.0, dr: 0.0,
      model: model } }

gameLogic :: ShipControl -> Game -> Game
gameLogic inputs gs = gs { ship = shipLogic inputs gs.ship }

type EffRuntime eff = Eff (webgl :: WebGl, dom :: DOM, timer :: TIMER, console :: CONSOLE | eff) Unit

main :: forall e. EffRuntime e
main = do
  log "Starting Spaceship"
  runWebGL "main-canvas" log
    \context -> do
      withShaders shaders log (shaderContext context)


shaderContext :: forall eff. WebGLContext -> ProgramBindings -> EffRuntime eff
shaderContext context bindings = do
  let bound = {
        context: context,
        webGLProgram: bindings.webGLProgram,
        aVertexPosition: bindings.aVertexPosition,
        uPMatrix: bindings.uPMatrix,
        uMVMatrix: bindings.uMVMatrix}
  clearColor 0.0 0.0 0.0 1.0
  enable DEPTH_TEST

  frames <- animationFrame
  control <- controls
  init <- initialState
  let game = foldp gameLogic init (sampleOn frames control)
  let boundRender = render bound
  runSignal (boundRender <$> game)
