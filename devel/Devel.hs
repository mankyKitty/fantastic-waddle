module Devel where

import           Obelisk.Run

import           Backend
import           Frontend

import qualified System.Random as Rnd

main :: Int -> IO ()
main port = Rnd.getStdGen
  >>= run port backend . frontend

