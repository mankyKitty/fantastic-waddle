{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
module WebGL.GOL where

import           Control.Lens                        (Lens', (.~), (^.), (^?))
import           Control.Monad.Except                (runExceptT)

import           Control.Monad                       ((>=>))

import           Data.Bool                           (bool)
import           Data.Function                       ((&))
import           Data.Semigroup                      ((<>))

import           System.Random                       (StdGen)
import qualified System.Random                       as Rnd

import           Reflex                              as R
import           Reflex.Dom.Core                     (Widget, (=:))
import qualified Reflex.Dom.Core                     as RD

import           GHCJS.DOM.Types                     (Float32Array, GLsizei,
                                                      MonadJSM, WebGLProgram,
                                                      WebGLRenderingContext,
                                                      WebGLTexture)

import qualified GHCJS.DOM.Types                     as GHCJS

import qualified GHCJS.DOM.WebGLRenderingContextBase as GLB

import           Reflex.Dom.CanvasBuilder.Types      (CanvasConfig (..), CanvasInfo (_canvasInfo_context))
import qualified Reflex.Dom.CanvasDyn                as C

import qualified Styling.Bootstrap                   as B

import           Internal                            (tshow, (<$$))

import           WebGL.Types                         (Error, GOL (..),
                                                      HasGOL (..))
import qualified WebGL.Types                         as GLT

import qualified WebGL.Internal                      as GLI
import qualified WebGL.Shaders.GOL                   as Shaders

data GOLInfo t = GOLInfo
  { _golReset :: Event t ()
  , _golStep  :: Event t ()
  , _golAuto  :: Dynamic t (Event t ())
  , _golCx    :: Dynamic t WebGLRenderingContext
  }

quad2 :: [Double]
quad2 = [-1, -1, 1, -1, -1, 1, 1, 1]

swap :: GOL -> GOL
swap g = g
  & golFront .~ (g ^. golBack)
  & golBack .~ (g ^. golFront)

scale,height,width :: Num a => a
scale = 4
width = 1024
height = 512

scaledHeight,scaledWidth :: Integral n => n
scaledHeight = height `div` scale
scaledWidth = width `div` scale

viewsize, statesize :: [Double]
statesize = [width/scale, height/scale]
viewsize = [width, height]

renderUsing
  :: MonadJSM m
  => GLsizei
  -> GLsizei
  -> Lens' GOL WebGLTexture
  -> Lens' GOL WebGLProgram
  -> Lens' GOL Float32Array
  -> WebGLRenderingContext
  -> GOL
  -> m GOL
renderUsing w h texL prgL sizeL cx g = do
  GLB.activeTexture cx GLB.TEXTURE0
  GLB.bindTexture cx GLB.TEXTURE_2D (g ^? texL)

  GLB.viewport cx 0 0 w h

  GLB.useProgram cx (g ^? prgL)
  GLI.attrib "quad" (g ^. golQuad) 2 Nothing prgL g cx
  GLI.uniformI "state" (0::Int) prgL g cx
  GLI.uniform2fv "scale" (g ^. sizeL) prgL g cx

  GLB.drawArrays cx GLB.TRIANGLE_STRIP 0 4
  pure g

stepRenderCopyTexture
  :: MonadJSM m
  => WebGLRenderingContext
  -> GOL
  -> m GOL
stepRenderCopyTexture cx g = do
  GLB.bindFramebuffer cx GLB.FRAMEBUFFER (g ^? golFrameBufferA)
  GLB.framebufferTexture2D cx GLB.FRAMEBUFFER GLB.COLOR_ATTACHMENT0 GLB.TEXTURE_2D (g ^? golBack) 0
  f <$> renderUsing scaledWidth scaledHeight golFront golGOLProgram golStateSize cx g
  where
    f g' = g'
      & golFront .~ (g' ^. golBack)
      & golBack .~ (g' ^. golFront)
      & golFrameBufferB .~ (g' ^. golFrameBufferA)
      & golFrameBufferA .~ (g' ^. golFrameBufferB)

step
  :: MonadJSM m
  => WebGLRenderingContext
  -> GOL
  -> m GOL
step cx g = do
  GLB.bindFramebuffer cx GLB.FRAMEBUFFER (g ^? golFrameBufferA)
  GLB.framebufferTexture2D cx GLB.FRAMEBUFFER GLB.COLOR_ATTACHMENT0 GLB.TEXTURE_2D (g ^? golBack) 0
  swap <$> renderUsing scaledWidth scaledHeight golFront golGOLProgram golStateSize cx g

draw
  :: MonadJSM m
  => WebGLRenderingContext
  -> GOL
  -> m GOL
draw cx g = do
  -- We're not rendering in 3D so turn it off
  GLB.disable cx GLB.DEPTH_TEST
  GLB.bindFramebuffer cx GLB.FRAMEBUFFER Nothing
  renderUsing width height golFront golCopyProgram golViewSize cx g

createGOL
  :: MonadJSM m
  => WebGLRenderingContext
  -> m (Either Error GOL)
createGOL cx =
  GHCJS.liftJSM . runExceptT . GLT.runWebGLM $ GLT.GOL
    <$> GLB.createFramebuffer cx
    <*> GLB.createFramebuffer cx
    <*> GLI.initTexture scaledWidth scaledHeight cx
    <*> GLI.initTexture scaledWidth scaledHeight cx
    <*> GLI.initProgram Shaders.golQuadVertSrc Shaders.golFragSrc cx
    <*> GLI.initProgram Shaders.golQuadVertSrc Shaders.golCopyFragSrc cx
    <*> GLI.createBuffer quad2 GLI.toFloat32Array cx
    <*> GLI.toFloat32Array statesize
    <*> GLI.toFloat32Array viewsize

setInitialState
  :: MonadJSM m
  => StdGen
  -> WebGLRenderingContext
  -> GOL
  -> m GOL
setInitialState sGen cx g = do
  let
    size = scaledWidth * scaledHeight
    on = [255,255,255,255]
    off = [0,0,0,225]

  u8arr <- GLI.toUint8Array $
    foldMap (bool on off) (take size (Rnd.randoms sGen))

  GLB.bindTexture cx GLB.TEXTURE_2D (g ^? golFront)

  GLI.texSubImage2DViewCustom cx GLB.TEXTURE_2D
    0 -- Level
    0 -- xOffset
    0 -- yOffset
    (width `div` scale)
    (height `div` scale)
    GLB.RGBA
    GLB.UNSIGNED_BYTE
    u8arr

  pure g

golError
  :: RD.MonadWidget t m
  => Error
  -> m ()
golError err =
  RD.divClass "error-wrapper" .
    RD.divClass "error-message" $
      RD.text (tshow err)

golRender
  :: ( RD.MonadWidget t m
     , RD.DomRenderHook t m
     )
  => GOLInfo t
  -> StdGen
  -> GOL
  -> m ()
golRender golInfo sGen gol' = mdo
  dGOL <- R.holdDyn gol' $ R.leftmost
    [ eStepRendered
    , eWasReset
    ]

  let
    glRun f eGo = RD.requestDomAction $ R.current
      ((\c -> f c >=> draw c) <$> _golCx golInfo <*> dGOL)
      <@ eGo

  eStepRendered <- glRun step $ R.switchDyn (_golAuto golInfo)
  eWasReset <- glRun (setInitialState sGen) $ _golReset golInfo

  pure ()

gol :: StdGen -> Widget x ()
gol sGen = RD.divClass "gol" $ do
  ePost <- RD.getPostBuild
  eTick <- () <$$ RD.tickLossyFromPostBuildTime 0.1

  eReset    <- B.bsButton_ "Reset" B.Secondary
  eStepOnce <- B.bsButton_ "Step" B.Primary
  eGoAuto   <- B.bsButton_ "Toggle Animation" B.Primary

  dTick <- fmap (bool eStepOnce eTick) <$> R.toggle False eGoAuto

  (canvas, _) <- RD.divClass "canvas-wrapper" $ RD.elAttr' "canvas"
    ( "height" =: tshow (height :: Int) <>
      "width"  =: tshow (width :: Int) <>
      "class"  =: "gol-canvas"
    )
    RD.blank

  dCx <- fmap _canvasInfo_context <$>
    C.dContextWebgl (CanvasConfig canvas mempty)

  (eError, eGol) <- fmap R.fanEither . RD.requestDomAction $
    R.current (createGOL <$> dCx) <@ ePost

  eDrawn <- RD.requestDomAction $
    R.current ((\c -> setInitialState sGen c >=> draw c) <$> dCx) <@> eGol

  _ <- RD.widgetHold (RD.text "Nothing Ready Yet.") $ R.leftmost
    [ golError                                            <$> eError
    , golRender (GOLInfo eReset eStepOnce dTick dCx) sGen <$> eDrawn
    ]

  pure ()

