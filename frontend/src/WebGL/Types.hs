{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK prune #-}
module WebGL.Types where

import           Control.Lens           (makeClassy, makeWrapped)
import           Control.Monad.Except   (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO)

import           Data.Text              (Text)

import           GHCJS.DOM.Types        (Float32Array, GLenum, JSM, MonadJSM,
                                         WebGLBuffer, WebGLFramebuffer,
                                         WebGLProgram, WebGLRenderingContext,
                                         WebGLTexture)
import           Linear                 (V3)
import           Reflex.Dom.Core        (Dynamic, Event)

newtype FragSrc = FragSrc
  { unFragSrc :: Text
  }
makeWrapped ''FragSrc

newtype VertSrc = VertSrc
  { unVertSrc :: Text
  }
makeWrapped ''VertSrc

data GOL = GOL
  { _golFrameBufferA :: WebGLFramebuffer
  , _golFrameBufferB :: WebGLFramebuffer
  , _golFront        :: WebGLTexture
  , _golBack         :: WebGLTexture
  , _golGOLProgram   :: WebGLProgram
  , _golCopyProgram  :: WebGLProgram
  , _golQuad         :: WebGLBuffer
  , _golStateSize    :: Float32Array
  , _golViewSize     :: Float32Array
  }
makeClassy ''GOL

newtype CubeRotation = CubeRot
  { unCubeRot :: Double
  }
  deriving (Eq, Show, Num)
makeWrapped ''CubeRotation

data GOLCube = GOLCube
  { _golCubeGOL            :: GOL
  , _golCubeProgram        :: WebGLProgram
  , _golCubeSqBuffer       :: WebGLBuffer
  , _golCubeTxBuffer       :: WebGLBuffer
  , _golCubeIxBuffer       :: WebGLBuffer
  , _golCubeProjMatTexture :: Float32Array
  , _golCubeProjMatPrimary :: Float32Array
  , _golCubeCubeRotation   :: CubeRotation
  }

data CubeInfo t = CubeInfo
  { _cubeInfoCx         :: Dynamic t WebGLRenderingContext
  , _cubeInfoReset      :: Event t ()
  , _cubeInfoTick       :: Dynamic t (Event t ())
  , _cubeInfoToggleAnim :: Event t ()
  , _cubeInfoPost       :: Event t ()
  , _cubeInfoMovePress  :: Event t (V3 Double -> V3 Double)
  }
makeClassy ''CubeInfo

data Error
  = ShaderCompileError GLenum (Maybe Text)
  | ProgramCompileError (Maybe Text)
  deriving Show

newtype WebGLM a = WebGLM
  { runWebGLM :: ExceptT Error JSM a
  }
  deriving ( Functor
           , Applicative
           , Monad
#ifdef ghcjs_HOST_OS
           , MonadIO
#else
           , MonadIO
           , MonadJSM
#endif
           , MonadError Error
           )

liftGLM :: Either Error a -> WebGLM a
liftGLM = either throwError pure
