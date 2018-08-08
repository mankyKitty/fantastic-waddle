{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
module WebGL.GOLCube
  ( golCube
  ) where

import           Control.Lens                        (Getting, Lens, Lens',
                                                      makeClassy, over, mapped, (.~),
                                                      traverseOf, (+~), (^.),
                                                      (^?), _Just, _Wrapped)
import           Control.Monad                       ((>=>))
import           Control.Monad.Except                (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class              (liftIO)

import           Data.Bits                           ((.|.))
import           Data.Bool                           (bool)
import           Data.Function                       ((&))
import           Data.Semigroup                      ((<>))
import           System.Random                       (StdGen)

import           Reflex                              (Dynamic, Event, (<@),
                                                      (<@>))
import qualified Reflex                              as R
import           Reflex.Dom.Core                     (Widget, (=:))
import qualified Reflex.Dom.Core                     as RD

import           Linear.Matrix                       ((!*!))
import qualified Linear.Matrix                       as LM
import           Linear.V3                           (V3 (..))
import qualified Linear.V4                           as V4

import qualified Linear.Projection                   as LP
import qualified Linear.Quaternion                   as Q

import           GHCJS.DOM.Types                     (Float32Array (..),
                                                      GLboolean, GLenum, GLint,
                                                      GLintptr, GLsizei, GLuint,
                                                      JSM, MonadJSM,
                                                      WebGLBuffer, WebGLProgram,
                                                      WebGLRenderingContext,
                                                      WebGLTexture, liftJSM)
import qualified GHCJS.DOM.WebGLRenderingContextBase as Gl
import           Language.Javascript.JSaddle         (valToStr)
import           Reflex.Dom.CanvasBuilder.Types      (CanvasConfig (..),
                                                      CanvasInfo (..))
import qualified Reflex.Dom.CanvasDyn                as C

import qualified Styling.Bootstrap                   as B

import           Internal                            (tshow, (<$$), (<$$>))
import           WebGL.GOL                           as GOL
import           WebGL.Internal                      as GI
import qualified WebGL.Shaders.GOLCube               as Shaders
import           WebGL.Statics                       (cubePositions,
                                                      flatColours, indices,
                                                      wrappedTex)
import           WebGL.Types                         (CubeRotation (..), Error,
                                                      GOL (..), GOLCube (..),
                                                      HasGOL (..),
                                                      HasGOLCube (..), liftGLM,
                                                      runWebGLM)
-- |
-- Rework of the example at <https://github.com/mdn/webgl-examples/blob/gh-pages/tutorial/sample6/webgl-demo.js Textured Cube Example>

fov = 45 * (pi / 180)
zNear = 0.1
zFar = 100.0

projectionMatrix :: Double -> LM.M44 Double
projectionMatrix aspect = LP.perspective fov aspect zNear zFar

modelViewMatrix :: LM.M44 Double
modelViewMatrix = LM.identity

golCubeDraw
  :: MonadJSM m
  => WebGLRenderingContext
  -> GOLCube
  -> m GOLCube
golCubeDraw cx g = do
  drawScene
    GOL.width
    GOL.height
    golFront
    golCubeProgram
    golViewSize
    golCubeProjMatPrimary
    cx g

drawScene
  :: MonadJSM m
  => GLsizei
  -> GLsizei
  -> Lens' GOL WebGLTexture
  -> Lens' GOLCube WebGLProgram
  -> Lens' GOL Float32Array
  -> Lens' GOLCube Float32Array
  -> WebGLRenderingContext
  -> GOLCube
  -> m GOLCube
drawScene w h texL prgL sizeL projL cx g = do
  Gl.bindFramebuffer cx Gl.FRAMEBUFFER Nothing

  Gl.clearColor cx 0.0 0.0 0.0 1.0 -- Clear to black, fully opaque
  Gl.clearDepth cx 1.0             -- Clear everything
  Gl.enable cx Gl.DEPTH_TEST       -- Enable depth testing
  Gl.depthFunc cx Gl.LEQUAL        -- Near things obscure far things
  Gl.clear cx (Gl.COLOR_BUFFER_BIT .|. Gl.DEPTH_BUFFER_BIT) -- Clear the canvas before we start drawing on it.
  let
    cubeRot = g ^. golCubeCubeRotation . _Wrapped
    tVector = V3 (-0.0) 0.0 (-4.0)
    zRotate = Q.axisAngle (V3 0 0 1) cubeRot
    xRotate = Q.axisAngle (V3 1 0 0) (cubeRot * 0.7)

    mvMat = modelViewMatrix
      !*! LM.mkTransformation zRotate tVector
      !*! LM.mkTransformation xRotate tVector

  -- Tell WebGL how to find the vertices of our cube from the position buffer
  GI.attrib "aVertexPosition" (g ^. golCubeSqBuffer) 3 Nothing prgL g cx
  GI.attrib "aTextureCoord" (g ^. golCubeTxBuffer) 2 Nothing prgL g cx

  Gl.bindBuffer cx Gl.ELEMENT_ARRAY_BUFFER (g ^? golCubeIxBuffer)

  Gl.useProgram cx (g ^? prgL)

  mvMat32Array <- GI.matToF32Array mvMat
  GI.uniformMatrix4fv "uModelViewMatrix" mvMat32Array prgL g cx
  GI.uniformMatrix4fv "uProjectionMatrix" (g ^. projL) prgL g cx

  GI.uniformI "state" (0::Int) prgL g cx

  Gl.activeTexture cx Gl.TEXTURE0
  Gl.bindTexture cx Gl.TEXTURE_2D (g ^? golCubeGOL . texL)

  Gl.viewport cx 0 0 w h
  let
    vertCount = 36
    vType     = Gl.UNSIGNED_SHORT
    vOffset   = 0

  Gl.drawElements cx Gl.TRIANGLES vertCount vType vOffset

  pure (g & golCubeCubeRotation . _Wrapped +~ 0.1)

createGOLCube
  :: MonadJSM m
  => WebGLRenderingContext
  -> m (Either Error GOLCube)
createGOLCube cx = liftJSM . runExceptT . runWebGLM $
  GOL.createGOL cx >>= liftGLM >>= f
  where
    f g = GOLCube
      <$> pure g
      <*> GI.initProgram Shaders.golCubeVertSrc Shaders.golCubeFragSrc cx
      <*> GI.createBuffer cubePositions GI.toFloat32Array cx
      <*> GI.createBuffer wrappedTex GI.toFloat32Array cx
      <*> GI.createBufferType Gl.ELEMENT_ARRAY_BUFFER indices GI.toUint16Array cx
      <*> GI.matToF32Array (projectionMatrix primAspect)
      <*> GI.matToF32Array (projectionMatrix texAspect)
      <*> pure (CubeRot 0.0)
      <*> GI.createBuffer flatColours GI.toFloat32Array cx

    primAspect = GOL.width / GOL.height
    texAspect = fromIntegral GOL.scaledWidth / fromIntegral GOL.scaledHeight

golCube :: StdGen -> Widget x ()
golCube sGen = RD.divClass "gol-cube" $ do
  ePost <- RD.getPostBuild
  eTick <- () <$$ RD.tickLossyFromPostBuildTime 0.1

  eReset      <- B.bsButton_ "Reset Game" B.Primary
  eStep       <- B.bsButton_ "Single Step" B.Primary
  eToggleAnim <- B.bsButton_ "Toggle Animation" B.Primary

  dTick <- bool eStep eTick <$$> R.toggle False eToggleAnim

  (canvas, _) <- RD.divClass "canvas-wrapper" $ RD.elAttr' "canvas"
    ( "height" =: tshow (height :: Int) <>
      "width"  =: tshow (width :: Int) <>
      "class"  =: "gol-canvas"
    )
    RD.blank

  dCx <- _canvasInfo_context <$$> C.dContextWebgl (CanvasConfig canvas [])

  (eError, eGol) <- R.fanEither <$$> RD.requestDomAction $
    R.current (createGOLCube <$> dCx) <@ ePost

  eInitialDraw <- RD.requestDomAction $
    (\c g -> do
        GOL.setInitialState sGen c (g ^. golCubeGOL)
        golCubeDraw c g
    )
    <$> R.current dCx
    <@> eGol

  rec dMGol <- R.holdDyn Nothing $ R.leftmost
        [ pure <$> eGol
        , eWasReset
        , eStepRendered
        ]

      eWasReset <- RD.requestDomAction $
        R.current ( (\cx ->
                       -- Set the rotation of the cube back to zero
                       -- (mapped . _Just . golCubeCubeRotation . _Wrapped .~ 0.0) .
                       -- Reset the Game of Life back to an initial random state
                       traverseOf (traverse . golCubeGOL) (GOL.setInitialState sGen cx)
                    )
                    <$> dCx
                    <*> dMGol
                  )
        <@ eReset

      eStepRendered <- RD.requestDomAction $
        R.current (
          (\cx -> traverse (traverseOf golCubeGOL (GOL.stepRenderCopyTexture cx) >=> golCubeDraw cx))
            <$> dCx
            <*> dMGol
          )
        <@ R.switchDyn dTick

  dStatus <- R.holdDyn "Nothing Yet" $ R.leftmost
    [ ("Bugger: " <>) . tshow <$> eError
    , "Woot!" <$ eGol
    ]

  dRendered <- R.holdDyn "Nothing Yet" $
    ("Rendered!" <$ eInitialDraw) <>
    ("Stepped!"  <$ eStepRendered)

  RD.divClass "status" (RD.dynText dStatus)
  RD.divClass "rendered" (RD.dynText dRendered)


  pure ()

