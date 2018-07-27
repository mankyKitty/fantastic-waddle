{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Canvas2D.TiledLines where

import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Bool (bool)
import Data.Semigroup ((<>))
import Data.Foldable (forM_)
import Data.Traversable (forM)

import Control.Monad ((>=>))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import System.Random (StdGen)
import qualified System.Random as Rnd

import qualified Reflex as R
import Reflex.Dom.Core (Widget, MonadWidget, (=:))
import qualified Reflex.Dom.Core as RD

import           GHCJS.DOM.Types                    (liftJSM,MonadJSM)
import qualified GHCJS.DOM.CanvasPath               as DOM_CP

import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as DOM_CR

import qualified Reflex.Dom.CanvasDyn               as C

import qualified Styling.Bootstrap as B
import qualified Canvas2D.Internal as CI

newtype Wid = Width { unWid :: Int }
  deriving (Show, Eq, Num)

newtype Hei = Height { unHei :: Int }
  deriving (Show, Eq, Num)

data LR = LtoR | RtoL
  deriving (Show, Eq)

size :: Int
size = 400

step :: Int
step = 100

cWid :: Wid
cWid = Width size

cHei :: Hei
cHei = Height size

canvasAttrs :: Map Text Text
canvasAttrs = 
  "width" =: (Text.pack . show . unWid $ cWid) <>
  "height" =: (Text.pack . show . unHei $ cHei)

ltor :: MonadJSM m => m LR
ltor = bool RtoL LtoR . (>= (0.5::Double)) <$> liftIO Rnd.randomIO

draw
  :: MonadJSM m
  => Int
  -> Int
  -> Int
  -> CanvasRenderingContext2D
  -> m ()
draw x y s cx = 
  let
    x' = fromIntegral x
    y' = fromIntegral y

    s' = fromIntegral s

    ln a b c d = do
      DOM_CP.moveTo cx a b 
      DOM_CP.lineTo cx c d

  in liftJSM $ ltor >>= \case
      LtoR -> ln x' y' (x' + s') (y' + s')
      RtoL -> ln (x' + s') y' x' (y' + s')

decSize,incSize :: Int -> Int
incSize = (+10)

decSize n =
  let
    n' = n - 10
  in 
    if n' == 0 then n else n'

tiledLines
  :: Widget x ()
tiledLines = do
  ePost <- RD.getPostBuild

  let
    drawSteps stp cx = do
      DOM_CR.clearRect cx 0 0 (fromIntegral size) (fromIntegral size)
      DOM_CR.beginPath cx
      forM_ [0,stp .. size] $ \x -> 
        forM  [0,stp .. size] $ \y ->
          draw x y stp cx

      DOM_CR.stroke cx

  (dStep, eStep) <- RD.divClass "row" $ do
    eInc <- B.bsButton_ "+ Step" B.Primary
    eDec <- B.bsButton_ "- Step" B.Primary

    dStep <- R.foldDyn ($) step $ R.mergeWith (.)
      [ incSize <$ eInc
      , decSize <$ eDec
      ]
    
    _ <- RD.dynText $ ("Step: " <>) . (Text.pack . show) <$> dStep

    pure (dStep, eInc <> eDec)

  dCx <- RD.divClass "row" $ 
    CI.createCanvasForCx canvasAttrs
  
  _ <- C.nextFrameAsyncWithCx dCx 
    (drawSteps <$> dStep) 
    (ePost <> eStep)

  pure ()
