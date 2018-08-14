{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Canvas2D.TiledLines where

import           Control.Lens                       ((^.), to, mapped)

import           Text.Read                          (readMaybe)

import           Data.Map                           (Map)

import Debug.Trace (traceShowId)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import Data.Text.Lens (_Text)

import           Data.Bool                          (bool)
import           Data.Foldable                      (forM_)
import           Data.Maybe                         (fromMaybe)
import           Data.Semigroup                     ((<>))
import           Data.Traversable                   (forM)

import           Control.Monad.IO.Class             (liftIO)
import qualified System.Random                      as Rnd

import qualified Reflex                             as R
import           Reflex.Dom.Core                    (Event, Reflex, Widget,
                                                     (<@), (=:))
import qualified Reflex.Dom.Core                    as RD

import qualified GHCJS.DOM.CanvasPath               as DOM_CP
import           GHCJS.DOM.Enums                    (CanvasWindingRule (..))
import           GHCJS.DOM.Types                    (JSString, MonadJSM,
                                                     liftJSM)

import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as DOM_CR

import qualified Reflex.Dom.CanvasDyn               as C

import           Internal                           (tshow, (<$$>))

import qualified Canvas2D.Internal                  as CI
import qualified Styling.Bootstrap                  as B

data Capped
  = Cap
  | NoCap
  deriving (Show, Eq)

newtype Wid = Width { unWid :: Int }
  deriving (Show, Eq, Num)

newtype Hei = Height { unHei :: Int }
  deriving (Show, Eq, Num)

data LR = LtoR | RtoL
  deriving (Show, Eq)

size :: Int
size = 600

step :: Int
step = 600

cWid :: Wid
cWid = Width size

cHei :: Hei
cHei = Height size

canvasAttrs :: Map Text Text
canvasAttrs =
  "width" =: (tshow . unWid $ cWid) <>
  "height" =: (tshow . unHei $ cHei)

ltor :: MonadJSM m => m LR
ltor = bool RtoL LtoR . (>= (0.5::Double)) <$> liftIO Rnd.randomIO

draw
  :: MonadJSM m
  => Int
  -> Int
  -> Int
  -> Capped
  -> CanvasRenderingContext2D
  -> m ()
draw x y s cap'd cx =
  let
    x' = fromIntegral x
    y' = fromIntegral y

    s' = fromIntegral s

    ln a b c d col r = do
      DOM_CP.moveTo cx a b
      DOM_CP.lineTo cx c d

      if Cap == cap'd then do
        DOM_CR.setFillStyle cx (col :: JSString)
        DOM_CP.ellipse cx (realToFrac c) (realToFrac d)
          (realToFrac (s' * 0.5))
          (realToFrac (s' * 0.2))
          (r * pi/180)
          0
          (2 * pi)
          False
        DOM_CR.fill cx (Just CanvasWindingRuleNonzero)
        else pure ()

  in liftJSM $ ltor >>= \case
      LtoR -> ln x' y' (x' + s') (y' + s') "lightseagreen" 45
      RtoL -> ln (x' + s') y' x' (y' + s') "palevioletred" (-45)

decSize,incSize :: Int -> Int -> Int
incSize s n = if (n + s) > step then n else n + s
decSize s n = if (n - s) <= 0 then n else n - s

tiledLines :: Widget x ()
tiledLines = do
  ePost <- RD.getPostBuild
  let
    defStepSize = 10

    drawSteps stp cap cx = do
      DOM_CR.clearRect cx 0 0 (fromIntegral size) (fromIntegral size)
      DOM_CR.beginPath cx

      forM_ [0,stp .. size] $ \x ->
        forM  [0,stp .. size] $ \y ->
          draw x y stp cap cx

      DOM_CR.stroke cx

  dStepSize <- RD._textInput_value
    <$> B.bsNumberInput "Step Size" "step-size" defStepSize

  let
    toEStepSize e = R.fmapMaybe (readMaybe . Text.unpack) $ R.current dStepSize <@ e

  (dStep, dCap, eStep) <- B.contained $ do
    eInc <- B.bsButton_ "+ Step" B.Primary
    eDec <- B.bsButton_ "- Step" B.Primary
    eCap <- B.bsButton_ "Toggle Ellipses" B.Secondary

    dCapped <- bool NoCap Cap <$$> RD.toggle False eCap

    dStep <- R.foldDyn ($) step $ R.mergeWith (.)
      [ incSize <$> toEStepSize eInc
      , decSize <$> toEStepSize eDec 
      ]

    pure (dStep, dCapped, eInc <> eDec)

  dCx <- B.contained $ do
    RD.dynText $ ("Step: " <>) . tshow <$> dStep
    CI.createCanvasForCx canvasAttrs

  _ <- RD.requestDomAction
    $ R.current (drawSteps <$> dStep <*> dCap <*> dCx)
    <@ ( ePost <> eStep )

  pure ()
