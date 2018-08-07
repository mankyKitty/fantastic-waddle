{-# LANGUAGE OverloadedStrings #-}
module WebGL.Shaders.GOL
  ( golFragSrc
  , golCopyFragSrc
  , golQuadVertSrc
  ) where

import           Data.Text      (Text)
import qualified Data.Text      as Text

import           Data.Semigroup ((<>))

setFloatPrecision :: Text
setFloatPrecision = Text.unlines
  [ "#ifdef GL_ES"
  , "precision mediump float;"
  , "#endif"
  ]

golQuadVertSrc :: Text
golQuadVertSrc = setFloatPrecision <> Text.unlines
  [ "attribute vec2 quad;"
  , "void main() {"
  , "    gl_Position = vec4(quad, 0, 1.0);"
  , "}"
  ]

golCopyFragSrc :: Text
golCopyFragSrc = setFloatPrecision <> Text.unlines
  [ "uniform sampler2D state;"
  , "uniform vec2 scale;"
  , "void main() {"
  , "    gl_FragColor = texture2D(state, gl_FragCoord.xy / scale);"
  , "}"
  ]

golFragSrc :: Text
golFragSrc = setFloatPrecision <> Text.unlines
  [ "uniform sampler2D state;"
  , "uniform vec2 scale;"
  , ""
  , "int get(int x, int y) {"
  , "    return int(texture2D(state, (gl_FragCoord.xy + vec2(x, y)) / scale).r);"
  , "}"
  , ""
  , "void main() {"
  , "    int sum = get(-1, -1) +"
  , "              get(-1,  0) +"
  , "              get(-1,  1) +"
  , "              get( 0, -1) +"
  , "              get( 0,  1) +"
  , "              get( 1, -1) +"
  , "              get( 1,  0) +"
  , "              get( 1,  1);"
  , ""
  , "    if (sum == 3) {"
  , "        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);"
  , "    } else if (sum == 2) {"
  , "        float current = float(get(0, 0));"
  , "        gl_FragColor = vec4(current, current, current, 1.0);"
  , "    } else {"
  , "        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);"
  , "    }"
  , "}"
  ]
