{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend
  ( frontend
  , headStatic
  , body
  ) where

import           System.Random        (StdGen)

import           Data.Semigroup       ((<>))
import           Data.Text            (Text)

import           Reflex.Dom.Core

import           WebGL.GOL            (gol)
import           WebGL.GOLCube        (golCube)

import           Canvas2D.JoyDivision (joyDivision)
import           Canvas2D.TiledLines  (tiledLines)

import           SVG.Squares          (squares)

import           Internal             ((<$$))
import qualified Styling.Bootstrap    as B

import           Static

widgetActivate :: MonadWidget t m => m () -> Text -> m (Event t (m ()))
widgetActivate w txt = w <$$ B.bsButton_ txt B.Primary

body :: StdGen -> Widget x ()
body sGen = do
  divClass "container" $
    divClass "row" $ do
    eWidgs <- divClass "col-2" $ sequenceA
      [ widgetActivate (squares sGen) "Squares"
      , widgetActivate (tiledLines sGen) "Tiled Lines"
      , widgetActivate (joyDivision sGen) "Joy Division"
      , widgetActivate (gol sGen) "Game Of Life"
      , widgetActivate (golCube sGen) "Cube Of Life (Spicy!)"
      ]

    _ <-
      divClass "col-10"
      . divClass "container"
        . divClass "row" $ widgetHold (tiledLines sGen) (leftmost eWidgs)
    blank
  blank

cssLink :: DomBuilder t m => Text -> m ()
cssLink ss = elAttr "link" ("rel" =: "stylesheet" <> "href" =: ss) blank

headStatic :: StaticWidget x ()
headStatic = do
  cssLink (static @"css/reflexive-art.css")
  cssLink (static @"css/bootstrap.min.css")
  el "title" $ text "Oh my"

frontend :: StdGen -> (StaticWidget x (), Widget x ())
frontend sGen =
  ( headStatic
  , body sGen
  )
