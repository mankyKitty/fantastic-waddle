{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend
  ( frontend
  , headStatic
  , body
  ) where

import           Control.Applicative  (liftA2)

import           System.Random        (StdGen)

import           Data.Semigroup       ((<>))
import           Data.Text            (Text)

import           Reflex.Dom.Core

import           Canvas2D.JoyDivision (joyDivision)
import Canvas2D.TiledLines (tiledLines)
import           Squares              (squares)

import qualified Styling.Bootstrap    as B

body :: StdGen -> Widget x ()
body sGen = do
  divClass "container" $
    divClass "row" $ do
    (eSq, eTl) <- divClass "col-2" $ liftA2 (,)
      (B.bsButton_ "Squares" B.Primary)
      (B.bsButton_ "Tiled Lines" B.Primary)
      -- (B.bsButton_ "Joy Division" B.Primary)

    let
      -- jd = joyDivision sGen
      sq = squares sGen
      tl = tiledLines

    _ <- divClass "col-10" 
      . divClass "container" 
        . divClass "row" $ 
          widgetHold sq $ leftmost 
            [ sq <$ eSq
            , tl <$ eTl
            -- , jd <$ eJd
            ]
    blank
  blank

headStatic :: StaticWidget x ()
headStatic = do
  cssLink "css/reflexive-art.css"
  cssLink "css/bootstrap.min.css"
  el "title" $ text "Oh my"
  where
    cssLink ss = elAttr "link" ("rel" =: "stylesheet" <> "href" =: ss) blank

frontend :: StdGen -> (StaticWidget x (), Widget x ())
frontend sGen =
  ( headStatic
  , body sGen 
  )
