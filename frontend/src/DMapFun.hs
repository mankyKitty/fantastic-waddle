{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
--
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
module DMapFun where

import Numeric (showFFloatAlt)

import           Control.Lens          (At (..), Index (..), IxValue, Ixed (..), LensLike',
                                        Lens', Traversal', Setter', makeLenses,
                                        makeWrapped, mapped, over, to, (%~), (?~), (.~), (%%~),
                                        (&), _Wrapped, (<&>))
import           Data.Dependent.Map
import qualified Data.Dependent.Map    as DM
import           Data.GADT.Compare.TH  (deriveGCompare, deriveGEq)

import           Data.Foldable         (Foldable, fold)
import           Data.Functor.Identity (Identity (..))

import           Data.Bool             (bool)
import           Data.Text             (Text, pack)

import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Reflex
import           Reflex.Dom            (DomBuilder, DomBuilderSpace,
                                        GhcjsDomSpace, RangeInput, (=:))
import qualified Reflex.Dom            as RD


alterF
  :: ( GCompare k
     , Functor f
     , Functor g
     )
  => k v
  -> (Maybe (g v) -> f (Maybe (g v)))
  -> DMap k g
  -> f (DMap k g)
alterF k f dm =
  f (DM.lookup k dm) <&> \v' -> DM.alter (const v') k dm

type family ValF w :: * -> *
type family KeyF w :: * -> *

class DIxed w where
  dix :: KeyF w v -> Traversal' w (ValF w v)

  default dix :: (Applicative f, DAt w) => KeyF w v -> LensLike' f w (ValF w v)
  dix k = dat k . traverse

class DIxed w => DAt w where
  dat :: KeyF w v -> Lens' w (Maybe (ValF w v))

type instance ValF (DMap k f) = f
type instance KeyF (DMap k f) = k

instance (Functor f, GCompare k) => DIxed (DMap k f) where
instance (Functor f, GCompare k) => DAt (DMap k f) where
  dat k f = alterF k f

-- | Basic Attribute Map
--
-- Designed to be the base for all the other attribute types (text,range,svg,etc)
--
newtype AttrMap (a :: * -> *) = AttrMap
  { unAttrMap :: DMap a Identity
  }
makeWrapped ''AttrMap

defaultAttrMap :: GCompare a => AttrMap a
defaultAttrMap = AttrMap empty

setAttr :: GCompare a => a v -> v -> AttrMap a -> AttrMap a
setAttr k v = over _Wrapped (insert k (Identity v))

delAttr :: GCompare a => a v -> AttrMap a -> AttrMap a
delAttr k = over _Wrapped (delete k)

-- We need to go back to (Map Text Text) for Reflex.Dom at some point.
toTextMap
  :: GCompare k
  => (forall v. k v -> v -> (Text,Text))
  -> AttrMap k
  -> Map Text Text
toTextMap f = foldrWithKey
  (\k v -> uncurry Map.insert (f k (runIdentity v)))
  mempty
  . unAttrMap

-- | End basic AttrMap

-- Helper for mooshing stringys
showt :: Show a => a -> Text
showt = pack . show

showf :: RealFloat f => f -> Text
showf f = pack $ showFFloatAlt Nothing f ""

-- |
-- DMap for generic range input attributes
--
data AC
  = On
  | Off
  | Default
  deriving (Eq)

instance Show AC where
  show On      = "on"
  show Off     = "off"
  show Default = "default"

data RangeAttr a where
  Min          :: RangeAttr Float
  Max          :: RangeAttr Float
  Req          :: RangeAttr Bool
  AutoComplete :: RangeAttr AC
  Name         :: RangeAttr Text
  ReadOnly     :: RangeAttr Bool
  Step         :: RangeAttr Float
  Value        :: RangeAttr Text

deriveGEq ''RangeAttr
deriveGCompare ''RangeAttr

newtype RangeInputAttrs = RangeInputAttrs
  { unRangeInputAttrs :: AttrMap RangeAttr
  }
makeWrapped ''RangeInputAttrs

-- |
-- I would like something better for this but I'm not that clever yet.
-- But the user never has to see this, so it shouldn't be a problem.
showTagged :: RangeAttr v -> v -> (Text, Text)
showTagged Min          v = ("min", showf v)
showTagged Max          v = ("max", showf v)
showTagged Req          v = ("required", showt v)
showTagged Name         v = ("name", showt v)
showTagged Step         v = ("step", showf v)
showTagged Value        v = ("value", showt v)
showTagged ReadOnly     v = ("readonly", bool "false" "true" v)
showTagged AutoComplete v = ("autocomplete", showt v)

toAttrMap :: RangeInputAttrs -> Map Text Text
toAttrMap = toTextMap showTagged . unRangeInputAttrs

data RInpConf t = RInpConf
  { _rInpConf_initialValue :: Float
  , _rInpConf_setValue     :: Event t Float
  , _rInpConf_attributes   :: Dynamic t RangeInputAttrs
  }
makeLenses ''RInpConf

rInpAttrs :: Reflex t => Setter' (RInpConf t) (DMap RangeAttr Identity)
rInpAttrs = rInpConf_attributes . mapped . _Wrapped . _Wrapped

defRInpConf :: Reflex t => RInpConf t
defRInpConf = RInpConf 0 never (pure $ RangeInputAttrs defaultAttrMap)

rangeInput'
  :: ( DomBuilder t m
     , PostBuild t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => RInpConf t
  -> m (RangeInput t)
rangeInput' (RInpConf initVal setVal dAttrs) =
  RD.rangeInput (RD.RangeInputConfig initVal setVal (toAttrMap <$> dAttrs))

usingIt :: RD.MonadWidget t m => m ()
usingIt = do
  let
    w = defRInpConf
      & rInpConf_attributes . mapped . _Wrapped . _Wrapped . dat Min ?~ 1.0 -- Fine
      -- & rInpConf_attributes . mapped . _Wrapped . _Wrapped . dat Max ?~ "Fudge" -- Type error!!
      -- frontend/src/DMapFun.hs:173:51: error:
      --     • Could not deduce (Data.String.IsString Float)
      --         arising from the literal ‘"Fudge"’
      --       from the context: RD.MonadWidget t m
      --         bound by the type signature for:
      --                    usingIt :: RD.MonadWidget t m => m ()
      --         at frontend/src/DMapFun.hs:168:1-37
      --     • In the second argument of ‘(?~)’, namely ‘"Fudge"’
      --       In the second argument of ‘(&)’, namely
      --         ‘rInpConf_attributes . mapped . dat Max ?~ "Fudge"’
      --       In the expression:
      --         defRInpConf & rInpConf_attributes . mapped . dat Min ?~ 1.0
      --         & rInpConf_attributes . mapped . dat Max ?~ "Fudge"

    -- defs = defRInpConf
    --   & rInpConf_attributes . mapped . _Wrapped %~ setAttr Min 1.0 -- This is fine!
    --   & rInpConf_attributes . mapped . _Wrapped %~ setAttr AutoComplete On -- Also fine...

      -- & rInpConf_attributes . mapped . _Wrapped %~ setAttr Max "1.0" -- Type Error!! awwww yissss
      -- frontend/src/DMapFun.hs:136:64: error:
      --   • Could not deduce (Data.String.IsString Float)
      --       arising from the literal ‘"1.0"’
      --     from the context: RD.MonadWidget t m
      --       bound by the type signature for:
      --                  usingIt :: RD.MonadWidget t m => m ()
      --       at frontend/src/DMapFun.hs:131:1-37
      --   • In the second argument of ‘setAttr’, namely ‘"1.0"’
      --     In the second argument of ‘(%~)’, namely ‘setAttr Max "1.0"’
      --     In the second argument of ‘(&)’, namely
      --       ‘rInpConf_attributes . mapped . _Wrapped %~ setAttr Max "1.0"’

  rInp <- rangeInput' w

  pure ()
