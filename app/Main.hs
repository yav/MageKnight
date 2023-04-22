module Main where

import Data.Text(Text)
import Data.Text qualified as Text

import Common.Interact
import Common.CallJS(jsHandlers)
import Common.RNGM
import Common.Field
import Common.Basics
import AppTypes

import Util.Perhaps

import Common
import Source
import ManaPool
import Deed(wound,deedName,deedColor)
import DeedDecks(makeDeckFor, spells)
import Hand
import Utils
import Land
import Terrain(openMap3)
import Deed.Decks(playDeed)

main :: IO ()
main =
  startApp App
  { appOptions = []
  , appColors = [ "red" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \rng _opts ps ->
      withRNG_ rng
      do src  <- newSource 6
         mb <- setupLand (defaultLandSetup openMap3 5 5 [1])
         pure
           case mb of
             Failed msg -> Left (Text.unpack msg)
             Ok (la,_mon) ->
               case ps of
                 [p] -> Right State { playerId  = p
                                    , _source   = src
                                    , _sourceUsed = False
                                    , _hand     = newHand deck
                                    , _mana     = emptyManaPool
                                    , _land     = la
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
      $ setField source (takeMana m sourceVal)
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
  available    = availableMana sourceVal




getSelectedCardOptions :: TopInputOptions
getSelectedCardOptions s =
  case getField handSelected handVal of
    Nothing -> []
    Just selected ->
      let deed   = getField selectedDeed selected
          mode   = getField selectedMode selected
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

      | mode /= SelectedAdvanced
      , not (null colors)
      ]
      ++
      [ topOpt s AskSelectedSideways "Turn sideways"
      $ setAndContinue
      $ setField hand (handSelectMode SelectedSideways handVal)
      $ s
      | mode == SelectedBasic
      ]
      ++
      [ topOpt s (ActionButton "Play") "Play card"
        do playDeed mode (deedName deed)
           updateThe_ hand (setField handSelected Nothing)
           sync
           gameLoop
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

