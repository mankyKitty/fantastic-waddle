{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Canvas2D.JoyDivision where

import           Prelude                            (Double, Float, Int, Num,
                                                     abs, div, floor,
                                                     fromIntegral, max,
                                                     realToFrac, show, snd, (*),
                                                     (+), (-), (/))

import           Control.Applicative                (liftA3, pure, (<*>))
import           Control.Category                   ((.))
import           Control.Lens                       (at, itraverse_, ix, mapped,
                                                     (%~), (+~), (?~), (^.),
                                                     (^?))
import           Control.Monad                      ((>>=))

import           Control.Monad.State                (evalState, gets, put)

import           Data.Foldable                      (forM_, traverse_)
import           Data.Traversable                   (traverse)

import           Data.Function                      (($), (&))
import           Data.Functor                       (fmap, ($>), (<$), (<$>))
import           Data.Maybe                         (fromMaybe)
import           Data.Semigroup                     ((<>))

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Map                           (Map)

import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V

import           Linear.V2                          (V2)
import qualified Linear.V2                          as L

import           System.Random                      (StdGen)
import qualified System.Random                      as Rnd

import qualified Reflex                             as R

import           Reflex.Dom.Core                    (MonadWidget, RangeInput,
                                                     Widget, (=:))
import qualified Reflex.Dom.Core                    as RD

import qualified Reflex.Dom.CanvasDyn               as C

import qualified GHCJS.DOM.CanvasPath               as DOM_CP
import           GHCJS.DOM.Types                    (JSM, MonadJSM, liftJSM)

import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as DOM_CR

import qualified Canvas2D.Internal                  as CI
import           Internal                           (tshow)
import qualified Styling.Bootstrap                  as B

newtype Lines = Lines
  { unLines :: Vector (Vector (V2 Double))
  }

paintingSize :: Num n => n
paintingSize = 600

paintingMargin :: Num n => n
paintingMargin = 20

step :: Int
step = floor ((paintingSize / paintingMargin) :: Double)

canvasSize :: Num n => n
canvasSize = paintingSize + paintingMargin

canvasAttrs :: Map Text Text
canvasAttrs =
  "class"  =: "canvas-elem" <>
  "height" =: s <>
  "width"  =: s
  where
    s = tshow (canvasSize :: Int)

initialLines :: Lines
initialLines =
  let
    n = paintingSize `div` step
  in
    Lines
    . V.iterateN n (fmap (L._y +~ fromIntegral step))
    $ V.iterateN n (L._x +~ fromIntegral step) (L.V2 paintingMargin paintingMargin)

applyWibbleWithVariance
  :: Lines
  -> Float
  -> Float
  -> Float
  -> StdGen
  -> Lines
applyWibbleWithVariance (Lines xs) l u vary =
  Lines . evalState ((traverse . traverse) f xs)
  where
    f v =
      let
        distToCentre = abs $ (v ^. L._x) - paintingSize / 2
        variance     = max (paintingSize / 2 - vary - realToFrac distToCentre) 0
      in
        gets (Rnd.randomR (l,u))
          >>= \(a,b) -> put b $> a
          >>= \r -> pure ( v & L._y +~ realToFrac (r * variance / 2 * (-1)) )

quadCurves
  :: MonadJSM m
  => CanvasRenderingContext2D
  -> Vector (V2 Double)
  -> m ()
quadCurves cx xs =
  let
    mv = V.take 1 xs
    cr = V.take (V.length xs - 2) (V.drop 1 xs)

    vIx vs i l = fromMaybe 0 (vs ^? ix i . l)

    drawCurve n v =
      let xc = ((v ^. L._x) + vIx xs (n + 2) L._x) / 2
          yc = ((v ^. L._y) + vIx xs (n + 2) L._y) / 2
      in
        DOM_CP.quadraticCurveTo cx (v ^. L._x) (v ^. L._y) xc yc

  in do
    traverse_ (\v -> DOM_CP.moveTo cx (v ^. L._x) (v ^. L._y)) mv
    itraverse_ drawCurve cr
    DOM_CP.quadraticCurveTo cx
      (vIx xs (V.length xs - 2) L._x)
      (vIx xs (V.length xs - 2) L._y)
      (vIx xs (V.length xs - 1) L._x)
      (vIx xs (V.length xs - 1) L._y)

straightLines
  :: MonadJSM m
  => CanvasRenderingContext2D
  -> Vector (V2 Double)
  -> m ()
straightLines cx =
  itraverse_ (\n v -> lineFn n cx (v ^. L._x) (v ^. L._y))
  where
    -- If this is the first point in a path, move there,
    -- otherwise continue drawing the line.
    lineFn 0 = DOM_CP.moveTo
    lineFn _ = DOM_CP.lineTo

type CurveFunction = CanvasRenderingContext2D -> Vector (V2 Double) -> JSM ()

drawLines
  :: MonadJSM m
  => Lines
  -> CurveFunction
  -> CanvasRenderingContext2D
  -> m ()
drawLines (Lines xxs) lineFn cx = liftJSM $ do
  -- Clear the rectangle so we don't leave drag marks everywhere, for now. ;)
  DOM_CR.clearRect cx 0 0 canvasSize canvasSize
  DOM_CR.setLineWidth cx 2
  -- Start the path
  DOM_CR.beginPath cx

  forM_ xxs $ \xs -> do
    lineFn cx xs
    DOM_CR.stroke cx

rangeConf :: R.Reflex t => Float -> Text -> RD.RangeInputConfig t
rangeConf i id' = RD.RangeInputConfig i R.never . pure
  $ "class" =: "range-input form-control"
  <> "id" =: id'

dRange
  :: MonadWidget t m
  => Text
  -> Text
  -> Float
  -> (Map Text Text -> Map Text Text)
  -> m (RangeInput t)
dRange lbl id' i f =
  RD.divClass "form-group" $ do
    RD.elAttr "label" ("for" =: id') $ RD.text lbl
    r <- RD.rangeInput $ rangeConf i id' & RD.rangeInputConfig_attributes . mapped %~ f
    RD.display (r ^. RD.rangeInput_value)
    pure r

joyDivision :: StdGen -> Widget x ()
joyDivision sGen = do
  ePost <- RD.getPostBuild

  (eWib, eStraight, eCurves) <- RD.elAttr "form" ("class" =: "form-inline") $ liftA3 (,,)
    (B.bsButton_ "Re-Wibble" B.Secondary)
    (B.bsButton_ "Straight Lines" B.Secondary)
    (B.bsButton_ "Quadratic Curves" B.Secondary)

  (dRLower, dRUpper, dRVariance) <- RD.el "form" $ liftA3 (,,)
    -- Lower bound range adjustment
    (RD.divClass "slider" $ dRange "Lower" "lower" 0 $ \m -> m
      & at "min" ?~ "-1"
      & at "max" ?~ "0"
      & at "step" ?~ "0.01"
    )
    -- Upper bound range adjustment
    (RD.divClass "slider" $ dRange "Upper" "upper" 0 $ \m -> m
      & at "min" ?~ "0"
      & at "max" ?~ "1"
      & at "step" ?~ "0.01"
    )
    -- Variance bound range adjustment
    (RD.divClass "slider" $ dRange "Variance" "variance" 50 $ \m -> m
      & at "min" ?~ "0"
      & at "max" ?~ tshow (paintingSize / 4 ::Double)
      & at "step" ?~ "1"
    )

  dCx <- CI.createCanvasForCx canvasAttrs

  dCurveFn <- R.holdDyn quadCurves $ R.leftmost
    [ straightLines <$ eStraight
    , quadCurves <$ eCurves
    ]

  let eBoop = ePost <> eWib

  dGen <- R.foldDyn ($) sGen $ R.mergeWith (.)
    [ snd . Rnd.next <$ eBoop
    ]

  let
    dWibble = applyWibbleWithVariance initialLines
      <$> (dRLower ^. RD.rangeInput_value)
      <*> (dRUpper ^. RD.rangeInput_value)
      <*> (dRVariance ^. RD.rangeInput_value)
      <*> dGen

  _ <- C.nextFrameAsyncWithCx dCx
    (drawLines <$> dWibble <*> dCurveFn)
    ( eBoop
      <> (() <$ dRLower ^. RD.rangeInput_input)
      <> (() <$ dRUpper ^. RD.rangeInput_input)
      <> (() <$ dRVariance ^. RD.rangeInput_input)
      <> eStraight
      <> eCurves
    )

  pure ()
