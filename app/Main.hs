module Main where

import Common.Interact
import Common.CallJS(jsHandlers)
import AppTypes

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red", "green", "blue", "yellow" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \_rng _opts _ps -> Right State
  , appStart = pure ()
  }
