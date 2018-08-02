{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo #-}
module WebGL.GOL
  ( gol
  ) where

import           Control.Lens                        (Lens', (.~), (^.), (^?))
import           Control.Monad.Except                (runExceptT)

import           Control.Monad                       ((>=>))

import           Data.Bool                           (bool)
import           Data.Foldable                       (fold)
import           Data.Function                       ((&))
import           Data.Semigroup                      ((<>))

import Data.Text (Text)
import qualified Data.Text                           as Text

import           System.Random                       (StdGen)
import qualified System.Random                       as Rnd

import           Reflex                              as R
import           Reflex.Dom.Core                     (Widget, (=:))
import qualified Reflex.Dom.Core                     as RD

import           GHCJS.DOM.Types                     (JSM,Float32Array, GLsizei,
                                                      MonadJSM, WebGLProgram,
                                                      WebGLRenderingContext,
                                                      WebGLTexture)

import qualified GHCJS.DOM.Types                     as GHCJS

import qualified GHCJS.DOM.WebGLRenderingContextBase as GLB

import           Reflex.Dom.CanvasBuilder.Types      (CanvasConfig (..),
                                                      CanvasInfo (_canvasInfo_context))
import qualified Reflex.Dom.CanvasDyn                as C

import qualified Styling.Bootstrap                   as B

import Internal ((<$$))

import WebGL.Types (GOL (..), Error, HasGOL (..))
import qualified WebGL.Types as GLT

import qualified WebGL.Internal as GLI
import qualified WebGL.Shaders.GOL                  as Shaders

tshow :: Show a => a -> Text
tshow = Text.pack . show

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

step
  :: MonadJSM m
  => WebGLRenderingContext
  -> GOL
  -> m GOL
step cx g = do
  GLB.bindFramebuffer cx GLB.FRAMEBUFFER (g ^? golFrameBuffer)
  GLB.framebufferTexture2D cx GLB.FRAMEBUFFER GLB.COLOR_ATTACHMENT0 GLB.TEXTURE_2D (g ^? golBack) 0
  swap <$> renderUsing scaledWidth scaledHeight golFront golGOLProgram golStateSize cx g

draw
  :: MonadJSM m
  => WebGLRenderingContext
  -> GOL
  -> m GOL
draw cx g = do
  GLB.bindFramebuffer cx GLB.FRAMEBUFFER Nothing
  renderUsing width height golFront golCopyProgram golViewSize cx g

createGOL
  :: MonadJSM m
  => WebGLRenderingContext
  -> m (Either Error GOL)
createGOL cx = do
  -- We're not rendering in 3D so turn it off
  GLB.disable cx GLB.DEPTH_TEST

  GHCJS.liftJSM . runExceptT . GLT.runWebGLM $ GLT.GOL
    <$> GLB.createFramebuffer cx
    <*> GLI.initTexture scaledWidth scaledHeight cx
    <*> GLI.initTexture scaledWidth scaledHeight cx
    <*> GLI.initProgram Shaders.golQuadVertSrc Shaders.golFragSrc cx
    <*> GLI.initProgram Shaders.golQuadVertSrc Shaders.golCopyFragSrc cx
    <*> GLI.createBuffer quad2 cx
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

runGL
  :: ( RD.DomRenderHook t m
     , RD.MonadWidget t m
     )
  => Dynamic t WebGLRenderingContext
  -> Dynamic t (Maybe GOL)
  -> (WebGLRenderingContext -> GOL -> JSM GOL)
  -> Event t a
  -> m (Event t (Maybe GOL))
runGL dCx dMGol glF eGo = RD.requestDomAction $
  (\c g -> traverse (glF c >=> draw c) g)
  <$> R.current dCx
  <*> R.current dMGol
  <@ eGo

gol :: StdGen -> Widget x ()
gol sGen = RD.divClass "gol" $ do
  ePost <- RD.getPostBuild
  eTick <- () <$$ RD.tickLossyFromPostBuildTime 0.1

  eReset <- B.bsButton_ "Reset" B.Secondary
  eStepOnce <- B.bsButton_ "Step" B.Primary
  eGoAuto <- B.bsButton_ "Auto" B.Primary

  dTick <- fmap (bool eStepOnce eTick) <$> R.toggle False eGoAuto

  (canvas, _) <- RD.divClass "canvas-wrapper" $ RD.elAttr' "canvas"
    ( "height" =: tshow (height :: Int) <>
      "width"  =: tshow (width :: Int) <>
      "class"  =: "gol-canvas"
    )
    RD.blank

  dCx <- fmap _canvasInfo_context <$>
    C.dContextWebgl (CanvasConfig canvas [])

  (eError, eGol) <- fmap R.fanEither <$> RD.requestDomAction $
    createGOL <$> R.current dCx <@ ePost

  dStatus <- R.holdDyn "Nothing Yet" $ R.leftmost
    [ ("Bugger: " <>) . tshow <$> eError
    , "Woot!" <$ eGol
    ]

  eDrawn <- RD.requestDomAction $
    (\c -> setInitialState sGen c >=> draw c) <$> R.current dCx <@> eGol

  -- Spicy!!
  rec dMGol <- R.holdDyn Nothing $ R.leftmost
        [ Just <$> eGol
        , eStepRendered
        , eWasReset
        ]

      eStepRendered <- runGL dCx dMGol step (R.switchDyn dTick)
      eWasReset <- runGL dCx dMGol (setInitialState sGen) eReset

  dRendered <- R.holdDyn "Nothing Yet" $
    ("Rendered!" <$ eDrawn) <>
    ("Stepped!"  <$ eStepRendered) <>
    ("Reset!"    <$ eWasReset)

  RD.divClass "status" (RD.dynText dStatus)
  RD.divClass "rendered" (RD.dynText dRendered)

  pure ()