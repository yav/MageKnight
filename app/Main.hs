module Main where

import Data.Set qualified as Set
import Data.Maybe(fromMaybe)
import Data.Text(Text)

import Common.Interact
import Common.CallJS(jsHandlers)
import Common.Utils(showText)
import Common.RNGM
import Common.Field
import Common.Basics
import AppTypes

import Common
import Source
import ManaPool
import Deed(wound,deedColor)
import DeedDecks(makeDeckFor, spells)
import Hand
import Utils

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \rng _opts ps ->
      case ps of
        [p] -> Right State { playerId  = p
                           , _source   = withRNG_ rng (newSource 6)
                           , _sourceUsed = False
                           , _hand     = newHand deck
                           , _mana     = emptyManaPool
                           }
        _   -> Left "need exactly 1 player"
  , appStart = gameLoop
  }
  where
  deck = wound : take 3 spells ++ take 5 (makeDeckFor "Arythea")


setAndContinue :: State -> Interact ()
setAndContinue s =
  do update (SetState s)
     gameLoop

getUseSourceInputOptions :: Interact [ TopInputOption ]
getUseSourceInputOptions =
  do s <- getState
     let pool         = getField mana s
         sourceVal    = getField source s
         available    = Set.toList (availableMana sourceVal)

     pure [ ( playerId s :-> Source m
            , "Use mana from the source" :: Text
            , setAndContinue
                $ setField source
                             (fromMaybe sourceVal (takeMana m sourceVal))
                $ setField sourceUsed True
                $ setField mana (addSourceMana m pool)
                  s
            )
          | not (getField sourceUsed s)
          , m <- available
          ]


getSelectedCardOptions :: Interact [ TopInputOption ]
getSelectedCardOptions =
  do s <- getState
     let opt a b c = (playerId s :-> a, b, c)
         handVal   = getField hand s
         pool      = getField mana s

     pure
       case getField handSelected handVal of
         Nothing -> []
         Just selected ->
           let deed   = getField selectedDeed selected
               colors = [ c | c <- map BasicMana (deedColor deed)
                            , hasMana 1 c pool ]
               -- XXX: time of day restrictions/gold
           in
           [ opt AskSelectedAdvanced "Use powered action"
             do mb <- chooseMaybe
                         (playerId s)
                         "Choose mana"
                         [ (AskMana m, "Power up") | m <- colors ]
                setAndContinue
                  case mb of
                    Just (AskMana m) ->
                      setField hand
                          (handSelectMode SelectedAdvanced handVal) $
                      setField mana (removeMana m pool) s
                    _ -> s

           | getField selectedMode selected /= SelectedAdvanced
           , not (null colors)
           ]
           ++
           [ opt AskSelectedSideways "Turn sideways"
           $ setAndContinue
           $ setField hand (handSelectMode SelectedSideways handVal)
           $ s
           | getField selectedMode selected == SelectedBasic
           ]


getPlayCardOptions :: Interact [ TopInputOption ]
getPlayCardOptions =
  do s <- getState
     let handVal = getField hand s
     pure
       case getField handSelected handVal of
         Just {} -> []
         Nothing -> [ (playerId s :-> AskHand i, "Select card",
                      setAndContinue (setField hand (handSelect i handVal) s)
                      )
                    | i <- handPlayable handVal
                    ]

gameLoop :: Interact ()
gameLoop =
  askInputs "Choose action" =<<
  fmap concat (sequence
                 [ getPlayCardOptions
                 , getSelectedCardOptions
                 , getUseSourceInputOptions
                 ])


oldGameLoop :: Interact ()
oldGameLoop =
  do s <- getState
     let p = playerId s
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



