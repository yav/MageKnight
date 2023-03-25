module Main where

import Data.Set qualified as Set

import Common.Interact
import Common.CallJS(jsHandlers)
import Common.Utils(showText)
import Common.RNGM
import Common.Field
import AppTypes

import Common
import Source
import Enemies(allEnemies)
import Deed(wound)
import DeedDecks(allDeeds, makeDeckFor, spells, advancedActions)
import Hand
import Utils

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \rng _opts ps ->
      case ps of
        [p] -> Right State { _playerId = p
                           , _source   = withRNG_ rng (newSource 6)
                           , _enemies  = allEnemies
                           , _deeds    = allDeeds
                           , _hand     = newHand deck
                           }
        _   -> Left "need exactly 1 player"
  , appStart = gameLoop
  }
  where
  deck = wound : take 3 spells ++ take 5 (makeDeckFor "Arythea")

gameLoop :: Interact ()
gameLoop =
  do s <- getState
     let p = getField playerId s
         src = getField source s
         avail = Set.toList (availableMana src)
         hnd = getField hand s

         opts0 = [ (TestReroll, "reroll") ] ++
            [ (TestFixed, "reroll") ] ++
            [ (Source m, showText m) | m <- avail ] ++
            [ (AskHand i, "Select card") | i <- handPlayable hnd ]

         opts = case getField handSelected hnd of
                   Nothing -> opts0
                   Just sel  ->
                     case getField selectedMode sel of
                       SelectedBasic ->
                          (AskSelectedAdvanced, "Power up") :
                          (AskSelectedSideways, "Use sideways") : opts0
                       _ -> opts0


     inp <- choose p "Choose Action" opts
     case inp of
       AskHand i -> updateThe_ hand (handSelect i)
       AskSelectedAdvanced -> updateThe_ hand (handSelectMode SelectedAdvanced)
       AskSelectedSideways -> updateThe_ hand (handSelectMode SelectedSideways)
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



