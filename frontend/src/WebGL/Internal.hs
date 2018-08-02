{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE OverloadedStrings #-}
module WebGL.Internal where

import Control.Monad (void, unless)
import Control.Lens (Lens', (^?), (^.))
import           Control.Monad.Except                (throwError)

import           GHC.Word                            (Word8)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import           GHCJS.DOM.Types                     (ArrayBuffer, JSString,
                                                      Float32Array, GLenum,
                                                      GLfloat, GLint, GLsizei,
                                                      JSVal, MonadJSM,
                                                      Uint8Array, WebGLBuffer,
                                                      WebGLProgram,
                                                      WebGLRenderingContext,
                                                      WebGLShader, WebGLTexture)

import qualified Language.Javascript.JSaddle.Object  as JSO

import qualified GHCJS.DOM.Types                     as GHCJS

import qualified GHCJS.DOM.WebGLRenderingContextBase as GLB

import WebGL.Types (GOL, WebGLM, Error (..))

initShader
  :: GHCJS.GLenum
  -> Text
  -> WebGLRenderingContext
  -> WebGLM WebGLShader
initShader sType source cx = do
  s <- GLB.createShader cx sType
  GLB.shaderSource cx (Just s) source
  GLB.compileShader cx (Just s)

  ok <- GHCJS.liftJSM . GHCJS.fromJSValUnchecked =<<
    GLB.getShaderParameter cx (Just s) GLB.COMPILE_STATUS

  unless ok $ do
    l <- GLB.getShaderInfoLog cx (Just s)
    throwError $ ShaderCompileError sType l

  pure s

initProgram
  :: Text
  -> Text
  -> WebGLRenderingContext
  -> WebGLM WebGLProgram
initProgram vs fs cx = do
  p <- GLB.createProgram cx
  vs' <- initShader GLB.VERTEX_SHADER vs cx
  fs' <- initShader GLB.FRAGMENT_SHADER fs cx
  GLB.attachShader cx (Just p) (Just vs')
  GLB.attachShader cx (Just p) (Just fs')

  GLB.linkProgram cx (Just p)

  ok <- GHCJS.liftJSM . GHCJS.fromJSValUnchecked =<<
    GLB.getProgramParameter cx (Just p) GLB.LINK_STATUS

  unless ok $ do
    l <- GLB.getProgramInfoLog cx (Just p)
    throwError (ProgramCompileError l)

  pure p

initTexture
  :: MonadJSM m
  => GLsizei
  -> GLsizei
  -> WebGLRenderingContext
  -> m WebGLTexture
initTexture x y cx = do
  t <- GLB.createTexture cx
  GLB.bindTexture cx GLB.TEXTURE_2D (Just t)

  GLB.texParameteri cx GLB.TEXTURE_2D GLB.TEXTURE_WRAP_S GLB.REPEAT
  GLB.texParameteri cx GLB.TEXTURE_2D GLB.TEXTURE_WRAP_T GLB.REPEAT

  GLB.texParameteri cx GLB.TEXTURE_2D GLB.TEXTURE_MIN_FILTER GLB.NEAREST
  GLB.texParameteri cx GLB.TEXTURE_2D GLB.TEXTURE_MAG_FILTER GLB.NEAREST

  GLB.texImage2DView cx
    GLB.TEXTURE_2D
    0
    GLB.RGBA
    x
    y
    0
    GLB.RGBA
    GLB.UNSIGNED_BYTE
    -- This texture is our working space, so initialise to "null"
    GHCJS.noArrayBufferView

  pure t

toUint8Array
  :: MonadJSM m
  => [Word8]
  -> m Uint8Array
toUint8Array ds = GHCJS.liftJSM $ do
  a <- JSO.new (JSO.jsg ("Uint8Array" :: JSString)) [ds]
  GHCJS.unsafeCastTo GHCJS.Uint8Array a

toFloat32Array
  :: MonadJSM m
  => [Double]
  -> m Float32Array
toFloat32Array ds = GHCJS.liftJSM $ do
  a <- JSO.new (JSO.jsg ("Float32Array" :: JSString)) [ds]
  GHCJS.unsafeCastTo GHCJS.Float32Array a

toArrayBuffer
  :: MonadJSM m
  => (array -> JSVal)
  -> array
  -> m ArrayBuffer
toArrayBuffer unwrap fa = GHCJS.liftJSM $ do
  b <- unwrap fa JSO.! ("buffer" :: JSString)
  GHCJS.unsafeCastTo GHCJS.ArrayBuffer b

createBuffer
  :: MonadJSM m
  => [Double]
  -> WebGLRenderingContext
  -> m WebGLBuffer
createBuffer arr cx = do
  ab <- toFloat32Array arr >>= toArrayBuffer GHCJS.unFloat32Array
  b <- GLB.createBuffer cx
  GLB.bindBuffer cx GLB.ARRAY_BUFFER (Just b)
  GLB.bufferData cx GLB.ARRAY_BUFFER (Just ab) GLB.STATIC_DRAW
  pure b

attrib
  :: MonadJSM m
  => Text
  -> WebGLBuffer
  -> GLint
  -> Maybe GLsizei
  -> Lens' GOL WebGLProgram
  -> GOL
  -> WebGLRenderingContext
  -> m ()
attrib nm val sz stride pl g cx = do
  aLoc <- GLB.getAttribLocation cx (g ^? pl) nm
  GLB.bindBuffer cx GLB.ARRAY_BUFFER (Just val)
  GLB.enableVertexAttribArray cx (fromIntegral aLoc)
  GLB.vertexAttribPointer cx (fromIntegral aLoc) sz GLB.FLOAT False (fromMaybe 0 stride) 0

uniform2fv
  :: MonadJSM m
  => Text
  -> Float32Array
  -> Lens' GOL WebGLProgram
  -> GOL
  -> WebGLRenderingContext
  -> m ()
uniform2fv nm v pL g cx = do
  uLoc <- GLB.getUniformLocation cx (g ^? pL) nm
  GLB.uniform2fv cx (Just uLoc) v

uniformF
  :: MonadJSM m
  => Text
  -> GLfloat
  -> Lens' GOL WebGLProgram
  -> GOL
  -> WebGLRenderingContext
  -> m ()
uniformF nm v pL g cx = do
  uLoc <- GLB.getUniformLocation cx (g ^? pL) nm
  GLB.uniform1f cx (Just uLoc) v

uniformI
  :: ( Integral n
     , MonadJSM m
     )
  => Text
  -> n
  -> Lens' GOL WebGLProgram
  -> GOL
  -> WebGLRenderingContext
  -> m ()
uniformI nm v pL g cx = do
  uLoc <- GLB.getUniformLocation cx (g ^? pL) nm
  GLB.uniform1i cx (Just uLoc) (fromIntegral v)

uniformB
  :: MonadJSM m
  => Text
  -> Bool
  -> Lens' GOL WebGLProgram
  -> GOL
  -> WebGLRenderingContext
  -> m ()
uniformB n b =
  uniformI n (bool (1::GLint) (0::GLint) b)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContextBase.texSubImage2D Mozilla WebGLRenderingContextBase.texSubImage2D documentation>
texSubImage2DViewCustom
  :: GHCJS.MonadDOM m
  => WebGLRenderingContext
  -> GLenum
  -> GLint
  -> GLint
  -> GLint
  -> GLsizei
  -> GLsizei
  -> GLenum
  -> GLenum
  -> Uint8Array
  -> m ()
texSubImage2DViewCustom self target level xoffset yoffset width height format type' pixels =
  GHCJS.liftDOM $ void
    (GHCJS.unWebGLRenderingContext self ^. JSO.jsf ("texSubImage2D" :: GHCJS.JSString)
      [ GHCJS.toJSVal target
      , GHCJS.toJSVal level
      , GHCJS.toJSVal xoffset
      , GHCJS.toJSVal yoffset
      , GHCJS.toJSVal width
      , GHCJS.toJSVal height
      , GHCJS.toJSVal format
      , GHCJS.toJSVal type'
      , GHCJS.toJSVal pixels
      ])