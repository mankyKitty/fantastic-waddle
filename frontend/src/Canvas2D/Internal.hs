{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Canvas2D.Internal
  ( createCanvasForCx
  , withStdGen
  ) where

import           Control.Monad.State                (gets, lift, put, runStateT)

import           Data.Map                           (Map)
import           Data.Text                          (Text)

import           System.Random                      (Random, StdGen)
import qualified System.Random                      as Rnd

import           Reflex.Dom.Core                    (Dynamic, MonadWidget)
import qualified Reflex.Dom.Core                    as RD

import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)

import qualified Reflex.Dom.CanvasBuilder.Types     as CTypes
import qualified Reflex.Dom.CanvasDyn               as C

createCanvasForCx
  :: MonadWidget t m
  => Map Text Text
  -> m (Dynamic t CanvasRenderingContext2D)
createCanvasForCx canvasAttrs = do
  (canvas, _) <- RD.divClass "canvas-wrapper" $
    RD.elAttr' "canvas" canvasAttrs RD.blank

  fmap CTypes._canvasInfo_context <$>
    C.dContext2d (CTypes.CanvasConfig canvas mempty)

withStdGen
  :: ( Monad m
     , Random n
     )
  => StdGen
  -> n
  -> n
  -> (n -> m a)
  -> m (a, StdGen)
withStdGen g l u f = flip runStateT g $ do
  (a,b) <- gets (Rnd.randomR (l,u))
  _ <- put b
  lift $ f a
