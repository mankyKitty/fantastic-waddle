module Backend where

import Common.Api
import Frontend

import qualified Obelisk.Backend as Ob
import qualified System.Random as Rnd

backend :: IO ()
backend = do
  sGen <- Rnd.getStdGen
  Ob.backend Ob.def
    { Ob._backendConfig_head = fst (frontend sGen)
    }
