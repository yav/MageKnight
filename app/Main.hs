module Main where

import Data.Set qualified as Set
import Data.Maybe(fromMaybe)
import Data.Text(Text)

import Common.Interact
import Common.CallJS(jsHandlers)
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


type TopInputOptions = State -> [ InputOption () ]

topOpt :: State -> Input -> Text -> Interact a -> InputOption a
topOpt s a b c = (playerId s :-> a, b, c)

setAndContinue :: State -> Interact ()
setAndContinue s =
  do update (SetState s)
     gameLoop

--------------------------------------------------------------------------------
getUseSourceInputOptions :: TopInputOptions
getUseSourceInputOptions s =
  [ topOpt s (Source m) "Use mana from the source" $
    setAndContinue
      $ setField source (fromMaybe sourceVal (takeMana m sourceVal))
      $ setField sourceUsed True
      $ setField mana (addSourceMana m pool)
        s
  | not (getField sourceUsed s)
  , m <- available
  ] ++
  [ topOpt s TestReroll "Reroll"
  $ setAndContinue
      $ setField source (refillSource sourceVal)
      $ setField mana emptyManaPool
      $ setField sourceUsed False s
  ]
  where
  pool         = getField mana s
  sourceVal    = getField source s
  available    = Set.toList (availableMana sourceVal)




getSelectedCardOptions :: TopInputOptions
getSelectedCardOptions s =
  case getField handSelected handVal of
    Nothing -> []
    Just selected ->
      let deed   = getField selectedDeed selected
          colors = [ c | c <- map BasicMana (deedColor deed)
                       , hasMana 1 c pool ]
      in
      [ topOpt s AskSelectedAdvanced "Use powered action"
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
      [ topOpt s AskSelectedSideways "Turn sideways"
      $ setAndContinue
      $ setField hand (handSelectMode SelectedSideways handVal)
      $ s
      | getField selectedMode selected == SelectedBasic
      ]
  where
  handVal   = getField hand s
  pool      = getField mana s


getPlayCardOptions :: TopInputOptions
getPlayCardOptions s =
  case getField handSelected handVal of
    Just {} -> []
    Nothing -> [ ( playerId s :-> AskHand i
                 , "Select card"
                 , setAndContinue (setField hand (handSelect i handVal) s)
                 )
               | i <- handPlayable handVal
               ]
  where
  handVal = getField hand s


getManaPoolOptions :: TopInputOptions
getManaPoolOptions s =
  [ topOpt s (AskManaPool Gold) "Convert to base mana"
  $ askInputs "Convert to" (map cvtTo anyBasicMana)
  | hasMana 1 Gold manaPool
    -- XXX: and day time
  ]

  where
  manaPool = getField mana s
  cvtTo b  =
    topOpt s (AskMana (BasicMana b)) "Convert to this color"
      $ setAndContinue
        $ setField mana (convertMana Gold (BasicMana b) manaPool) s



gameLoop :: Interact ()
gameLoop =
  do s <- getState
     askInputs "Choose action"
       [ opt
       | opts <-
          [ getPlayCardOptions
          , getSelectedCardOptions
          , getUseSourceInputOptions
          , getManaPoolOptions
          ]
       , opt <- opts s
       ]

