module Main where

import Common.Interact
import Common.CallJS(jsHandlers)
import Common.RNGM
import AppTypes

import Source

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \rng _opts ps ->
      case ps of
        [p] -> Right (State p (withRNG_ rng (newSource 6)))
        _   -> Left "need exactly 1 player"
  , appStart = gameLoop
  }

gameLoop :: Interact ()
gameLoop = pure ()


