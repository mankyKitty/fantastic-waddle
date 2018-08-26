{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Canvas2D.TiledLines where

import           GHC.Word                           (Word8)

import           Text.Read                          (readMaybe)

import           Data.Map                           (Map)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Bool                          (bool)
import           Data.Foldable                      (forM_)
import           Data.Semigroup                     ((<>))

import           Control.Monad.IO.Class             (MonadIO, liftIO)
import qualified Control.Monad.Random               as MRnd
import           System.Random                      (StdGen)
import qualified System.Random                      as Rnd

import qualified Reflex                             as R
import           Reflex.Dom.Core                    (Widget, (<@), (=:))
import qualified Reflex.Dom.Core                    as RD

import qualified GHCJS.DOM.CanvasPath               as DOM_CP
import           GHCJS.DOM.Types                    (MonadJSM, liftJSM)

import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as DOM_CR

import           Internal                           (tshow, (<$$>))

import qualified Canvas2D.Internal                  as CI
import qualified Styling.Bootstrap                  as B

data Weird
  = Weird
  | NoWeird
  deriving (Show, Eq)

newtype Wid = Width { unWid :: Int }
  deriving (Show, Eq, Num)

newtype Hei = Height { unHei :: Int }
  deriving (Show, Eq, Num)

data LR
  = LeftToRight
  | RightToLeft
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

ltor1 :: IO LR
ltor1 = MRnd.uniform [RightToLeft, LeftToRight]

ltor2 :: Rnd.StdGen -> (LR, Rnd.StdGen)
ltor2 = first (bool RightToLeft LeftToRight . (>= 0.5))
  . Rnd.randomR (0::Double,1.0)

ltor3 :: MonadIO m => m LR
ltor3 = bool RightToLeft LeftToRight . (>= (0.5::Double))
  <$> liftIO (Rnd.randomRIO (0::Double,1.0))

draw
  :: MonadJSM m
  => Int
  -> Int
  -> Int
  -> Weird
  -> CanvasRenderingContext2D
  -> m ()
draw x y s weird'd cx =
  let
    x' = fromIntegral x
    y' = fromIntegral y

    s' = fromIntegral s

    ln a b c d r = do
      DOM_CP.moveTo cx a b

      if Weird == weird'd then do
        let xc = (a + c + s' * r) / 2
            yc = (b + d + s' * r) / 2

        DOM_CP.quadraticCurveTo cx c d xc yc
        else DOM_CP.lineTo cx c d

  in liftJSM $ liftIO ltor1 >>= \case
      LeftToRight -> ln x' y' (x' + s') (y' + s') 1
      RightToLeft -> ln (x' + s') y' x' (y' + s') (-1)

decSize,incSize :: Int -> Int -> Int
incSize s n = if (n + s) > step then n else n + s
decSize s n = if (n - s) <= 0 then n else n - s

tiledLines :: StdGen -> Widget x ()
tiledLines _ = do
  ePost <- RD.getPostBuild
  let
    defStepSize :: Word8
    defStepSize = 30

    drawSteps stp cap cx = do
      DOM_CR.clearRect cx 0 0 (fromIntegral size) (fromIntegral size)
      DOM_CR.beginPath cx

      let xy = [0,stp .. size]

      forM_ xy $ \x ->
        forM_ xy $ \y ->
          draw x y stp cap cx

      DOM_CR.stroke cx

  dStepSize <- RD._textInput_value
    <$> B.bsNumberInput "Step Size" "step-size" defStepSize

  let
    eStepSize eTrigger =
      R.fmapMaybe (readMaybe . Text.unpack) $ R.current dStepSize <@ eTrigger

  (dStep, dWeird, eStep) <- B.contained $ do
    eInc <- B.bsButton_ "+ Step" B.Primary
    eDec <- B.bsButton_ "- Step" B.Primary
    eWeird <- B.bsButton_ "Toggle Weird" B.Secondary

    dWeird' <- bool NoWeird Weird <$$> RD.toggle False eWeird

    dStep <- R.foldDyn ($) (fromIntegral defStepSize) $ R.mergeWith (.)
      [ incSize <$> eStepSize eInc
      , decSize <$> eStepSize eDec
      ]

    pure (dStep, dWeird', eInc <> eDec <> eWeird)

  dCx <- B.contained $ do
    RD.dynText $ ("Step: " <>) . tshow <$> dStep
    CI.createCanvasForCx canvasAttrs

  _ <- RD.requestDomAction $
    R.current ( drawSteps
                <$> dStep
                <*> dWeird
                <*> dCx
              )
    <@ ( ePost <> eStep )

  pure ()
