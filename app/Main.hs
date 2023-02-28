module Main where

import Data.Set qualified as Set

import Common.Interact
import Common.CallJS(jsHandlers)
import Common.Utils(showText)
import Common.RNGM
import Common.Field
import AppTypes

import Debug.Trace
import Common
import Source
import Utils

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
  do s <- getState
     let p = getField playerId s
         src = getField source s
         avail = Set.toList (availableMana src)
     inp <- choose p "Choose Action"
          $ [ (TestReroll, "reroll") ] ++
            [ (TestFixed, "reroll") ] ++
            [ (Source m, showText m) | m <- avail ]
     case inp of
       TestReroll -> updateThe_ source refillSource
       TestFixed ->
          do inp1 <- choose p "Choose mana"
                                    [ (Source m, showText m) | m <- avail ]
             case inp1 of
               Source from ->
                 do to' <- choose p "Set to"
                          [ (AskMana to, "Convert to this") | to <- anyMana ]
                    case to' of
                      AskMana to
                        | Just s1 <- takeAndConvertMana from to src ->
                          setThe source s1
                      _ -> pure ()
               _ -> pure ()

       Source m | Just s1 <- takeMana m src -> setThe source s1

       _ -> pure ()

     sync
     gameLoop



