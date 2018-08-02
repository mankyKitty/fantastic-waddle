{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune #-}
module WebGL.Types where

import Control.Lens (makeClassy)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text)

import GHCJS.DOM.Types  ( GLenum 
                        , JSM
                        , Float32Array
                        , MonadJSM
                        , WebGLBuffer
                        , WebGLFramebuffer
                        , WebGLProgram
                        , WebGLTexture
                        )

data GOL = GOL
  { _golFrameBuffer :: WebGLFramebuffer
  , _golFront       :: WebGLTexture
  , _golBack        :: WebGLTexture
  , _golGOLProgram  :: WebGLProgram
  , _golCopyProgram :: WebGLProgram
  , _golQuad        :: WebGLBuffer
  , _golStateSize   :: Float32Array
  , _golViewSize    :: Float32Array
  }
makeClassy ''GOL

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