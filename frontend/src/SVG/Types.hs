{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module SVG.Types where

import           GHC.Word                    (Word8)

import           Control.Lens                (makeClassy, makeWrapped)

import           Data.Text                   (Text)

import           Reflex.Dom.Widget.SVG.Types (Height, SVG_Polygon (..), Width)

data World = World
  { _worldHeight :: Height
  , _worldWidth  :: Width
  }
makeClassy ''World

newtype Count = Count { unCount :: Int }
  deriving (Eq, Show)

newtype Size = Size { unSize :: Double }
  deriving (Eq, Show)

newtype Padding = Padding { unPadding :: Word8 }
  deriving (Eq, Show)

data StrokeFill
  = Stroke
  | Fill
  deriving (Eq, Show)

data Colour
  = Greenish
  | Orangeish
  | Redish
  deriving (Show, Eq)

toCssColour :: Colour -> Text
toCssColour Greenish  = "lightseagreen"
toCssColour Redish    = "palevioletred"
toCssColour Orangeish = "orange"

newtype PolyAttrs = PolyAttrs
  { unPolyAttrs :: (StrokeFill, Colour) }
  deriving (Eq, Show)
makeWrapped ''PolyAttrs

newtype Poly = Poly
  { unPoly :: (PolyAttrs, SVG_Polygon) }
  deriving (Eq,Show)
makeWrapped ''Poly
