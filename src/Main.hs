module Main where

import Data.Text(Text)
import Data.Text qualified as Text

import KOI.CallJS(jsHandlers)
import KOI.RNGM
import KOI.Field
import KOI.Basics
import AppTypes

import Util.Perhaps

import State
import Input
import Common
import Source
import ManaPool
import Deed(wound,deedName,deedColor)
import DeedDecks(makeDeckFor, spells)
import Hand
import Utils
import Land
import Hero
import Terrain(addrGlobal,MapShape(..))
import Deed.Decks(playDeed)

main :: IO ()
main =
  startApp App
  { appId = MK
  , appOptions = []
  , appColors = [ "red" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \rng _opts ps ->
      withRNG_ rng
      do src  <- newSource 6
         mb <- setupLand (defaultLandSetup Wedge 9 2 [11,11,11,11])
         pure
           case (ps,mb) of
             ([],_) -> Left "need a player"
             (_,Failed msg) -> Left (Text.unpack msg)
             (p:_, Ok (la,_mon)) ->
               Right State { playerId     = p
                           , playerHero   = hero
                           , _source      = src
                           , _sourceUsed  = False
                           , _hand        = newHand deck
                           , _mana        = emptyManaPool
                           , _land        = la
                           , _movement    = 0
                           , _phase       = MovePhase
                           }
  , appStart = gameLoop
  }
  where
  hero = Arythea
  deck = wound : makeDeckFor hero


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


getMoveOptions :: TopInputOptions
getMoveOptions s =
  [ topOpt s (AskLoc addr explo) lab
  $ setAndContinue
  $ updField land
    (\la ->
    case explo of
      Nothing -> setPlayer addr la
      Just _  ->
        case exploreAt (addrGlobal addr) la of
          Ok (l1,newMons) -> l1
          Failed {} -> la
    ) s
  | (addr,explo) <- getPlayerNeighbours (getField land s)
  , let lab = case explo of
                Nothing -> "Move here"
                Just _  -> "Explore here"
  ]


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
          , getMoveOptions
          ]
       , opt <- opts s
       ]
