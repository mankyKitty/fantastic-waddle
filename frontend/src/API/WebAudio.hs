module API.WebAudio
  ( checkNullOrUndefined
  , dNewAudioCtx
  , dNewAnalyser
  ) where

import           Control.Applicative         (liftA2)
import           Reflex.Dom.Core             (DomRenderHook, Dynamic, Event,
                                              MonadWidget)
import qualified Reflex.Dom.Core             as RD

import qualified GHCJS.DOM.AudioContext      as AC
import qualified GHCJS.DOM.Types             as DOM

import           GHCJS.DOM.Types             (AnalyserNode, AudioContext, JSM,
                                              MonadJSM, PToJSVal)

import qualified Language.Javascript.JSaddle as JS

checkNullOrUndefined
  :: ( MonadJSM m
     , PToJSVal a
     )
  => a
  -> m (Maybe a)
checkNullOrUndefined a =
  let
    null' = JS.ghcjsPure . JS.isNull . DOM.pToJSVal
    undef' = JS.ghcjsPure . JS.isUndefined . DOM.pToJSVal

    comp n u = if n || u then Nothing else pure a
  in
    DOM.liftJSM $ liftA2 comp (null' a) (undef' a)

dJSObj
  :: ( DomRenderHook t m
     , MonadWidget t m
     , PToJSVal b
     )
  => (a -> JSM b)
  -> Event t a
  -> m (Dynamic t (Maybe b))
dJSObj f eRq = do
  eA <- RD.requestDomAction ((checkNullOrUndefined =<<) . DOM.liftJSM . f <$> eRq)
  RD.holdDyn Nothing eA

dNewAudioCtx
  :: ( DomRenderHook t m
     , MonadWidget t m
     )
  => Event t ()
  -> m (Dynamic t (Maybe AudioContext))
dNewAudioCtx = do
  dJSObj (const AC.newAudioContext)

dNewAnalyser
  :: ( DomRenderHook t m
     , MonadWidget t m
     )
  => Event t AudioContext
  -> m (Dynamic t (Maybe AnalyserNode))
dNewAnalyser = do
  dJSObj AC.createAnalyser
