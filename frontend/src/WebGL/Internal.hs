{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module WebGL.Internal where

import           Control.Lens                        (Lens', (^.), (^?))
import           Control.Monad                       (unless, void)
import           Control.Monad.Except                (throwError)

import           Data.Bool                           (bool)
import           Data.Foldable                       (foldMap,
                                                      toList)
import           Data.Maybe                          (fromMaybe)
import           Data.Semigroup                      (mconcat, (<>))
import           Data.Text                           (Text)
import           GHC.Word                            (Word16, Word8)

import           Linear.Matrix                       (M44, transpose)
import           Linear.V4                           (V4 (..))

import           GHCJS.DOM.Types                     (ArrayBuffer, Float32Array,
                                                      GLenum, GLfloat, GLint,
                                                      GLsizei, IsGObject,
                                                      JSString, JSVal, MonadJSM,
                                                      PToJSVal, ToJSVal,
                                                      Uint16Array, Uint8Array,
                                                      WebGLBuffer, WebGLProgram,
                                                      WebGLRenderingContext,
                                                      WebGLShader, WebGLTexture)

import qualified Language.Javascript.JSaddle.Object  as JSO

import qualified GHCJS.DOM.Types                     as GHCJS

import qualified GHCJS.DOM.WebGLRenderingContextBase as GLB

import           WebGL.Types                         (Error (..), GOL, WebGLM)

matToF32Array
  :: MonadJSM m
  => M44 Double
  -> m Float32Array
matToF32Array =
  toFloat32Array
  . foldMap toList
  . transpose -- just an alias for distribute

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

toTypedArray
  :: ( ToJSVal a
     , IsGObject c
     , MonadJSM m
     )
  => JSString
  -> (JSVal -> c)
  -> a
  -> m c
toTypedArray t c ds = GHCJS.liftJSM $ do
  a <- JSO.new (JSO.jsg t) [ds]
  GHCJS.unsafeCastTo c a

toUint8Array
  :: MonadJSM m
  => [Word8]
  -> m Uint8Array
toUint8Array =
  toTypedArray "Uint8Array" GHCJS.Uint8Array

toUint16Array
  :: MonadJSM m
  => [Word16]
  -> m Uint16Array
toUint16Array =
  toTypedArray "Uint16Array" GHCJS.Uint16Array

toFloat32Array
  :: MonadJSM m
  => [Double]
  -> m Float32Array
toFloat32Array =
  toTypedArray "Float32Array" GHCJS.Float32Array

toArrayBuffer
  :: MonadJSM m
  => (array -> JSVal)
  -> array
  -> m ArrayBuffer
toArrayBuffer unwrap fa = GHCJS.liftJSM $ do
  b <- unwrap fa JSO.! ("buffer" :: JSString)
  GHCJS.unsafeCastTo GHCJS.ArrayBuffer b

createBufferType
  :: ( ToJSVal xs
     , IsGObject arr
     , PToJSVal arr
     , MonadJSM m
     )
  => GLenum
  -> xs
  -> (xs -> m arr)
  -> WebGLRenderingContext
  -> m WebGLBuffer
createBufferType btype arr arrFn cx = do
  ab <- arrFn arr >>= toArrayBuffer GHCJS.pToJSVal
  b <- GLB.createBuffer cx
  GLB.bindBuffer cx btype (Just b)
  GLB.bufferData cx btype (Just ab) GLB.STATIC_DRAW
  pure b

createBuffer
  :: ( ToJSVal xs
     , IsGObject arr
     , PToJSVal arr
     , MonadJSM m
     )
  => xs
  -> (xs -> m arr)
  -> WebGLRenderingContext
  -> m WebGLBuffer
createBuffer =
  createBufferType GLB.ARRAY_BUFFER

attrib
  :: MonadJSM m
  => Text
  -> WebGLBuffer
  -> GLint
  -> Maybe GLsizei
  -> Lens' a WebGLProgram
  -> a
  -> WebGLRenderingContext
  -> m ()
attrib nm val sz stride prgL a cx = do
  aLoc <- GLB.getAttribLocation cx (a ^? prgL) nm
  GLB.bindBuffer cx GLB.ARRAY_BUFFER (Just val)
  GLB.vertexAttribPointer cx (fromIntegral aLoc) sz GLB.FLOAT False (fromMaybe 0 stride) 0
  GLB.enableVertexAttribArray cx (fromIntegral aLoc)

uniformMatrix4fv
  :: MonadJSM m
  => Text
  -> Float32Array
  -> Lens' a WebGLProgram
  -> a
  -> WebGLRenderingContext
  -> m ()
uniformMatrix4fv nm v pL g cx = do
  uLoc <- GLB.getUniformLocation cx (g ^? pL) nm
  GLB.uniformMatrix4fv cx (Just uLoc) False v

uniform2fv
  :: MonadJSM m
  => Text
  -> Float32Array
  -> Lens' a WebGLProgram
  -> a
  -> WebGLRenderingContext
  -> m ()
uniform2fv nm v pL g cx = do
  uLoc <- GLB.getUniformLocation cx (g ^? pL) nm
  GLB.uniform2fv cx (Just uLoc) v

uniformF
  :: MonadJSM m
  => Text
  -> GLfloat
  -> Lens' a WebGLProgram
  -> a
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
  -> Lens' a WebGLProgram
  -> a
  -> WebGLRenderingContext
  -> m ()
uniformI nm v pL g cx = do
  uLoc <- GLB.getUniformLocation cx (g ^? pL) nm
  GLB.uniform1i cx (Just uLoc) (fromIntegral v)

uniformB
  :: MonadJSM m
  => Text
  -> Bool
  -> Lens' a WebGLProgram
  -> a
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
