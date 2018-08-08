{-# LANGUAGE OverloadedStrings #-}
module Styling.Bootstrap
  ( BSStyle (..)
  , styleClass
  , contained

  , bsButton
  , bsButton_
  ) where

import           Control.Lens    (mapped, over, _1)
import           Data.Char       (toLower)
import           Data.Semigroup  ((<>))
import           Data.Text       (Text, pack)

import           Reflex.Dom.Core (Event, MonadWidget, (=:))
import qualified Reflex.Dom.Core as RD

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
