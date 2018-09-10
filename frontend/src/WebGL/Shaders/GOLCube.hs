{-# LANGUAGE OverloadedStrings #-}
module WebGL.Shaders.GOLCube
  ( golCubeFragSrc
  , golCubeVertSrc
  ) where

import           Data.Text      (Text)
import qualified Data.Text      as Text

import           Data.Semigroup ((<>))

import           WebGL.Types    (FragSrc (..), VertSrc (..))

setFloatPrecision :: Text
setFloatPrecision = Text.unlines
  [ "#ifdef GL_ES"
  , "precision mediump float;"
  , "#endif"
  ]

golCubeVertSrc :: VertSrc
golCubeVertSrc = VertSrc $ setFloatPrecision <> Text.unlines
  [ "attribute vec4 aVertexPosition;"
  , "attribute vec2 aTextureCoord;"
  , ""
  , "uniform mat4 uModelViewMatrix;"
  , "uniform mat4 uProjectionMatrix;"
  , ""
  , "varying highp vec2 vColour;"
  , ""
  , "void main(void) {"
  , "  gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;"
  , "  vColour = aTextureCoord;"
  , "}"
  ]

golCubeFragSrc :: FragSrc
golCubeFragSrc = FragSrc $ setFloatPrecision <> Text.unlines
  [ "uniform sampler2D state;"
  , ""
  , "varying highp vec2 vColour;"
  , ""
  , "void main() {"
  , "  gl_FragColor = texture2D(state, vColour);"
  , "}"
  ]
