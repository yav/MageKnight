module Main where

import Data.Set qualified as Set

import Common.Interact
import Common.CallJS(jsHandlers)
import Common.Utils(showText)
import Common.RNGM
import AppTypes

import Debug.Trace

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
gameLoop =
  do State p s <- getState
     inp <- choose p "Choose Mana"
            [ (Source m, showText m) | m <- Set.toList (availableMana s) ]
     traceM (show inp)
     case inp of
       Source m | Just s1 <- takeMana m s ->
          do update (SetState (State p s1))
             traceM (show s1)

       _ -> do traceM (s `seq` ("Failed to take " ++ show inp))
               traceM (show (availableMana s))
               pure ()
     gameLoop



