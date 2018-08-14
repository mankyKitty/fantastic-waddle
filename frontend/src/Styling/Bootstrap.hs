{-# LANGUAGE OverloadedStrings #-}
module Styling.Bootstrap
  ( BSStyle (..)
  , styleClass
  , contained

  , bsButton
  , bsButton_

  , rangeInpConf
  , bsRangeInput
  , bsNumberInput
  ) where

import           Control.Lens    (mapped, over, _1, (%~))

import Data.Function ((&))
import           Data.Char       (toLower)
import           Data.Semigroup  ((<>))
import           Data.Text       (Text, pack)
import Data.Map (Map)

import           Reflex          (Reflex)
import qualified Reflex          as R

import           Reflex.Dom.Core (Event, RangeInput, MonadWidget, (=:))
import qualified Reflex.Dom.Core as RD

import           Internal        (tshow)

data BSStyle
  = Primary
  | Secondary
  | Success
  | Danger
  | Warning
  | Info
  | Light
  | Dark
  | Link
  deriving (Eq,Show)

styleClass :: BSStyle -> Text
styleClass = pack . (\(h:t) -> toLower h : t ) . show

bsButton :: MonadWidget t m => BSStyle -> m a -> m (Event t (), a)
bsButton sty child = over (mapped . _1) (RD.domEvent RD.Click)
  $ RD.elAttr' "button" ("class" =: ("m-2 btn btn-" <> styleClass sty) <> "type" =: "button") child

bsButton_ :: MonadWidget t m => Text -> BSStyle -> m (Event t ())
bsButton_ l sty = fst <$> bsButton sty (RD.text l)

contained :: MonadWidget t m => m a -> m a
contained = RD.divClass "row" . RD.divClass "container"

rangeInpConf
  :: Reflex t
  => Float
  -> Text
  -> RD.RangeInputConfig t
rangeInpConf i id' = RD.RangeInputConfig i R.never . pure
  $ "class" =: "custom-range"
  <> "id" =: id'
  <> "type" =: "range"

bsRangeInput
  :: MonadWidget t m
  => Text
  -> Text
  -> Float
  -> (Map Text Text -> Map Text Text)
  -> m (RangeInput t)
bsRangeInput lbl id' i f =
  RD.divClass "form-group" $ do
    RD.elAttr "label" ("for" =: id') $ RD.text lbl
    RD.rangeInput $ rangeInpConf i id' & RD.rangeInputConfig_attributes . mapped %~ f

bsNumberInput
  :: ( Show n
     , Num n
     , MonadWidget t m
     )
  => Text
  -> Text
  -> n
  -> m (RD.TextInput t)
bsNumberInput lbl id' i = RD.divClass "form-group" $ do
  RD.elAttr "label" ("for" =: id') $ RD.text lbl
  RD.textInput (RD.TextInputConfig "number" (tshow i) R.never (pure $ "id" =: id'))
