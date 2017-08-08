module Space.Render where
import Prelude
import Space.Spaceship (Game())
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Graphics.WebGLAll (Attribute, Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mat4, Mode(TRIANGLES), Shaders(Shaders), Uniform, Vec3, WebGLContext, WebGLProg, clear, drawArr, getCanvasHeight, getCanvasWidth, setUniformFloats, viewport)
import Data.Matrix4 (rotate, translate, identity, makePerspective) as M
import Data.Matrix (toArray) as M
import Data.Vector3 as V
import Data.Int (toNumber)

type ShaderBinds = {
  aVertexPosition :: Attribute Vec3,
  uPMatrix :: Uniform Mat4,
  uMVMatrix :: Uniform Mat4 }

type ContextBindings = {
  context :: WebGLContext,
  webGLProgram :: WebGLProg,
  aVertexPosition :: Attribute Vec3,
  uPMatrix :: Uniform Mat4,
  uMVMatrix :: Uniform Mat4 }

type ProgramBindings = {
  webGLProgram :: WebGLProg,
  aVertexPosition :: Attribute Vec3,
  uPMatrix :: Uniform Mat4,
  uMVMatrix :: Uniform Mat4 }


shaders :: Shaders ShaderBinds
shaders = Shaders
-- fragment
  """
  precision mediump float;
  void main(void) {
    gl_FragColor = vec4(0.5, 0.5, 1.0, 1.0);
  }
  """
-- vertex
  """
  attribute vec3 aVertexPosition;
  uniform mat4 uMVMatrix;
  uniform mat4 uPMatrix;
  void main(void) {
    gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
  }
  """

render :: forall eff. ContextBindings -> Game -> Eff (webgl :: WebGl | eff) Unit
render bindings game = do
  canvasWidth <- getCanvasWidth bindings.context
  canvasHeight <- getCanvasHeight bindings.context
  viewport 0 0 canvasWidth canvasHeight

  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

  let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
  setUniformFloats bindings.uPMatrix (M.toArray pMatrix)

  let mvMatrix = M.rotate (- game.ship.r) (V.vec3' [0.0, 0.0, 1.0])
                 $ M.translate (V.vec3 game.ship.y game.ship.x (-30.0)) M.identity
  setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

  drawArr TRIANGLES game.ship.model bindings.aVertexPosition

shipMesh :: Array Number
shipMesh = [0.0, 1.0, 0.0,
            (-1.0), (-1.0), 0.0,
            1.0, (-1.0), 0.0]
