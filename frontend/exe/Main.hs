module Main where

import Reflex.Dom (Widget, mainWidgetWithHead,mainWidget)

import System.Random (getStdGen)

import Frontend (body)

main
  :: IO ()
main = do
  initialGen <- getStdGen
  mainWidget (body initialGen)
