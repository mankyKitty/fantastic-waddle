{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Canvas2D.JoyDivision where

import           Prelude                            (Double, Float, Int, Num,
                                                     abs, div, floor,
                                                     fromIntegral, max,
                                                     realToFrac, snd, (*),
                                                     (+), (-), (/))

import           Control.Applicative                (liftA3, pure, (<*>))
import           Control.Category                   ((.))
import           Control.Lens                       (at, itraverse_, ix,
                                                     (+~), (?~), (^.),
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

import           Data.Map                           (Map)

import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V

import           Linear.V2                          (V2)
import qualified Linear.V2                          as L

import           System.Random                      (StdGen)
import qualified System.Random                      as Rnd

import qualified Reflex                             as R

import           Reflex.Dom.Core                    (Widget, (=:))
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

joyDivision :: StdGen -> Widget x ()
joyDivision sGen = do
  ePost <- RD.getPostBuild

  (eWib, eStraight, eCurves) <- B.contained $
    RD.elAttr "form" ("class" =: "form-inline") $ liftA3 (,,)
      (B.bsButton_ "Re-Wibble" B.Secondary)
      (B.bsButton_ "Straight Lines" B.Secondary)
      (B.bsButton_ "Quadratic Curves" B.Secondary)

  (dRLower, dRUpper, dRVariance) <- B.contained $
    RD.elAttr "form" ("class" =: "form-inline") $ liftA3 (,,)
    -- Lower bound range adjustment
    (RD.divClass "slider" $ B.bsRangeInput "Lower" "lower" 0 $ \m -> m
      & at "min" ?~ "-1"
      & at "max" ?~ "0"
      & at "step" ?~ "0.01"
    )
    -- Upper bound range adjustment
    (RD.divClass "slider" $ B.bsRangeInput "Upper" "upper" 0 $ \m -> m
      & at "min" ?~ "0"
      & at "max" ?~ "1"
      & at "step" ?~ "0.01"
    )
    -- Variance bound range adjustment
    (RD.divClass "slider" $ B.bsRangeInput "Variance" "variance" 50 $ \m -> m
      & at "min" ?~ "0"
      & at "max" ?~ tshow ((paintingSize / 2) :: Double)
      & at "step" ?~ "1"
    )

  dCx <- B.contained $ CI.createCanvasForCx canvasAttrs

  -- Maintain the function we use to draw our curves. Start with drawing curves.
  dCurveFn <- R.holdDyn quadCurves $ R.leftmost
    [ -- When one of the line buttons are clicked, the Event produces a function.
      straightLines <$ eStraight -- straight lines
    , quadCurves <$ eCurves      -- curves
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
