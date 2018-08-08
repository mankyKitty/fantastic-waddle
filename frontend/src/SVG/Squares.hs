{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ViewPatterns              #-}
module SVG.Squares
  ( squares
  ) where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         (at, mapped, over, to, (.~),
                                                       views, ( # ), (%~), (+~),
                                                       (?~), (^.), _1, _2,
                                                       _Wrapped)

import           Control.Monad                        (join, replicateM, void)
import           Control.Monad.Fix                    (MonadFix)
import           Control.Monad.Reader                 (MonadReader, runReaderT)

import           Control.Monad.Random                 (MonadRandom, Random,
                                                       RandomGen, StdGen)
import qualified Control.Monad.Random                 as Rnd

import Data.Function ((&))
import           Data.Semigroup                       (sconcat, (<>))

import           Data.Bifunctor                       (first)

import           Data.Map                             (Map)

import           Data.Text                            (Text)

import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as NE

import           Numeric.Noise                        (Seed)
import qualified Numeric.Noise.Perlin                 as Perlin

import           Reflex.Dom.Core                      (Dynamic, Event,
                                                       MonadHold, Reflex,
                                                       Widget, (=:), (<@))
import qualified Reflex.Dom.Core                      as RD

import qualified Reflex.Dom.Widget.SVG                as SVG
import           Reflex.Dom.Widget.SVG.Types          (Height, Pos,
                                                       SVG_Path (..),
                                                       SVG_Polygon (..), Width,
                                                       X, Y, _PosX, _PosY)
import qualified Reflex.Dom.Widget.SVG.Types          as SVGT
import qualified Reflex.Dom.Widget.SVG.Types.SVG_Path as P

import           Internal                             (tshow, (<$$))
import qualified Styling.Bootstrap                    as B

import           SVG.Types                            (Colour (..), Count (..),
                                                       HasWorld (..),
                                                       Padding (..), Poly (..),
                                                       PolyAttrs (..),
                                                       Size (..),
                                                       StrokeFill (..),
                                                       World (..), toCssColour)

createPositions
  :: Size
  -> Float
  -> Float
  -> (Pos X, Pos Y, Pos X, Pos Y)
createPositions (realToFrac . unSize -> sz) x y =
  ( _PosX # (x * 2)
  , _PosY # (y * 2)
  , _PosX # (sz + x * 2)
  , _PosY # (sz + y * 2)
  )

makeSquarePath
  :: Size
  -> Float
  -> Float
  -> SVG_Path
makeSquarePath sz x y =
  let
    (sX,sY,sPlusX,sPlusY) = createPositions sz x y
  in
    D $ P._M sX sY :|
    [ P._L sPlusX sY            -- a > b
    , P._L sPlusX sPlusY        -- b > c
    , P._L sX sPlusY            -- c > d
    , P._L sX sY                -- d > a
    ]

createShape
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     , Real n
     , Eq s
     )
  => Size
  -> Padding
  -> Count
  -> (n -> n -> s)
  -> m (NonEmpty s)
createShape _sz pad n f = do
  let
    pad' = unPadding pad
    toBnd x = (pad', floor x `div` 2 - pad')

    rndNum = fmap fromIntegral . Rnd.getRandomR

  wBnd <- views (worldWidth . _Wrapped) toBnd
  hBnd <- views (worldHeight . _Wrapped) toBnd

  let mk = f <$> rndNum wBnd <*> rndNum hBnd

  fmap NE.nub . (:|) <$> mk <*> replicateM (unCount n) mk

createPath
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     )
  => Size
  -> Padding
  -> Count
  -> m SVG_Path
createPath sz pad n =
  sconcat <$> createShape sz pad n (makeSquarePath sz)

createPolygons
  :: ( MonadRandom m
     , MonadReader r m
     , HasWorld r
     )
  => Size
  -> Padding
  -> Count
  -> m (NonEmpty Poly)
createPolygons sz pad n =
  let
    attrs = fmap PolyAttrs $ (,)
      <$> Rnd.weighted [ (Stroke, 0.4), (Fill, 0.6) ]
      <*> Rnd.uniform [ Greenish, Redish, Orangeish ]

    mkPoly x y =
      let
        (sX,sY,sPlusX,sPlusY) = createPositions sz x y
      in
        SVG_Polygon (sX,sY)
        $ (sPlusX, sY)
        :| [ (sPlusX, sPlusY)
           , (sX, sPlusY)
           , (sX, sY)
           ]
  in
    traverse (fmap Poly . liftA2 (,) attrs . pure) =<<
      createShape sz pad n mkPoly

generatePolys
  :: RandomGen g
  => World
  -> Size
  -> Padding
  -> Count
  -> g
  -> (NonEmpty Poly, g)
generatePolys wrld sz padding count' =
  Rnd.runRand (runReaderT (createPolygons sz padding count') wrld)

addNoise
  :: Seed
  -> Double
  -> (Pos X, Pos Y)
  -> (Pos X, Pos Y)
addNoise seed scale xy =
  let
    seed' = fromIntegral seed
    octaves = 5
    persistance = 0.5
    noise = Perlin.perlin seed octaves scale persistance

    x' = xy ^. _1 . _Wrapped . to realToFrac
    y' = xy ^. _2 . _Wrapped . to realToFrac

    noiseVal = realToFrac $
      Perlin.noiseValue noise (x' + seed', y' + seed', seed') - 0.5
  in
    xy & _1 . _Wrapped +~ noiseVal
       & _2 . _Wrapped +~ noiseVal

addPerlinNoiseToPoly
  :: Seed
  -> Double
  -> Poly
  -> Poly
addPerlinNoiseToPoly seed scale =
  over (_Wrapped . _2) (
    (SVGT.svg_polygon_start %~ addNoise seed scale)
  . (SVGT.svg_polygon_path . traverse %~ addNoise seed scale)
  )

makePolyProps
  :: Poly
  -> Map Text Text
makePolyProps poly =
  SVGT.makePolygonProps (poly ^. _Wrapped . _2)
  <> "class" =: "basic-poly"
  <> case poly ^. to unPoly . _1 . to unPolyAttrs of
    (Stroke, c) -> "stroke" =: toCssColour c <> "fill" =: "none"
    (Fill, c)   -> "fill"   =: toCssColour c

dRandomRange
  :: ( Reflex t
     , RandomGen g
     , Random a
     , MonadHold t m
     , MonadFix m
     )
  => g
  -> (a,a)
  -> (b -> a -> a)
  -> Event t b
  -> m (Dynamic t (a,g))
dRandomRange g bnds fn =
  RD.foldDyn (\b -> first (fn b) . Rnd.randomR bnds . snd) (fst bnds, g)

scaleShiftFn :: Double -> (Double -> Double) -> Double -> (Double -> Double)
scaleShiftFn step _ 0.1 = subtract step
scaleShiftFn step _ 0.0 = (+ step)
scaleShiftFn _    f _   = f

bgProps :: Map Text Text
bgProps = "class" =: "svg-wrapper"

wheight :: SVGT.Height
wheight = SVGT.Height 60

wwidth :: SVGT.Width
wwidth = SVGT.Width 60

svgEl :: SVGT.SVG_El
svgEl = SVGT.SVG_El wwidth wheight Nothing

decSqCount, incSqCount :: Count -> Count
decSqCount (Count 1)   = (Count 1)
decSqCount (Count n)   = (Count (n - 100))

incSqCount (Count 600) = (Count 600)
incSqCount (Count n)   = (Count (n + 100))

squares :: StdGen -> Widget x ()
squares sGen = do
  eTickInfo <- () <$$ RD.tickLossyFromPostBuildTime 0.016

  eGenerate <- B.bsButton_ "New Squares" B.Secondary
  eOnButton <- B.bsButton_ "Manual" B.Secondary
  eOnTick   <- B.bsButton_ "Auto" B.Secondary

  eFoof <- RD.switchHold RD.never $ RD.leftmost
    [ eGenerate <$ eOnButton
    , eTickInfo <$ eOnTick
    ]

  let
    scaleStep = 0.0001
    sF = uncurry (scaleShiftFn scaleStep)

  dScaleRange <- (fmap . fmap) snd . RD.foldDyn ($) ((+ scaleStep), 0.05) $
    (\s@(_, a) -> (sF s, sF s a) ) <$ eFoof

  let
    sqPadding = Padding 3
    sqCount   = Count 600
    wrld      = World wheight wwidth
    size      = Size 1.5

    dSvgRootAttrs = pure $
      bgProps <> SVGT.makeSVGProps svgEl

    genPolys sqC sG = generatePolys
      wrld size sqPadding sqC sG

  -- rec (dSqCount, eSqCountChg) <- B.contained $ do
  --       eIncSq <- B.bsButton_ "+ Sqr" B.Info
  --       _ <- RD.dynText $ mappend "Square Count: " . tshow . unCount <$> dSqCount
  --       eDecSq <- B.bsButton_ "- Sqr" B.Info

  --       dSqrs <- RD.foldDyn ($) sqCount $ RD.mergeWith (.)
  --         [ incSqCount <$ eIncSq
  --         , decSqCount <$ eDecSq
  --         ]

  --       pure (dSqrs, eIncSq <> eDecSq)

  rec (dPolys, dGen) <- RD.splitDynPure <$> RD.holdDyn (genPolys sqCount sGen)
        (RD.current (genPolys <$> pure sqCount <*> dGen) <@ eGenerate)

  dRandInt <- fmap fst <$> dRandomRange sGen (2,3) (const id) eGenerate


  dScaleInp <- RD.divClass "scale-slider" $ do
    RD.text "Scale"
    fmap (fmap realToFrac . (^. RD.rangeInput_value)) . RD.rangeInput $ RD.def
      & RD.rangeInputConfig_initialValue .~ 0.05
      & RD.rangeInputConfig_attributes . mapped %~ \m -> m
          & at "step" ?~ "0.0001"
          & at "min" ?~ "0.0"
          & at "max" ?~ "0.1"

  dFoofen <- RD.holdDyn dScaleInp $ RD.leftmost
    [ dScaleInp <$ eOnButton
    , dScaleRange <$ eOnTick
    ]

  let
    dPerlin = addPerlinNoiseToPoly
      <$> dRandInt
      <*> join dFoofen

  void . SVG.svgElDynAttr' SVG.SVG_Root dSvgRootAttrs $
    RD.simpleList (NE.toList <$> dPolys) (\dPoly ->
      SVG.svgBasicDyn_ SVG.Polygon makePolyProps (dPerlin <*> dPoly)
    )
