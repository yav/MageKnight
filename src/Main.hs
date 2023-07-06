module Main where

import Data.Text(Text)
import Data.Text qualified as Text
import Optics

import KOI.CallJS(jsHandlers)
import KOI.RNGM
import KOI.Basics

import Util.Perhaps

import Game.KOI
import Game.State
import Game.Input

import Mana.Type
import Mana.Source
import Mana.Pool
import Deed.Color(deedColor)
import Hand
import Utils
import Terrain.Map
import Terrain.Type(addrGlobal,MapShape(..))
import Hero
import Deed.Type
import Deed.Play(playDeed)
import Combat
import Enemies(orcs,guardians)

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
                           , _heal        = 0
                           , _phase       = ActionPhase (CombatAction (com la))
                           }
  , appStart = gameLoop
  }
  where
  hero = Arythea
  deck = Wound : makeDeckFor hero
  com la =
    startCombat
          [ BattleSite
              { siteId = playerLocation la
              , siteFortified = False
              , siteEnemies = take 1 orcs ++ take 1 guardians
              }
          ]



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
      $ set source (takeMana m sourceVal)
      $ set sourceUsed True
      $ set mana (addSourceMana m pool)
        s
  | not (view sourceUsed s)
  , m <- available
  ] ++
  [ topOpt s TestReroll "Reroll"
  $ setAndContinue
      $ set source (refillSource sourceVal)
      $ set mana emptyManaPool
      $ set sourceUsed False s
  ]
  where
  pool         = view mana s
  sourceVal    = view source s
  available    = availableMana sourceVal




getSelectedCardOptions :: TopInputOptions
getSelectedCardOptions s =
  case view handSelected handVal of
    Nothing -> []
    Just selected ->
      let deed   = view selectedDeed selected
          mode   = view selectedMode selected
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
                 set hand (handSelectMode SelectedAdvanced handVal) $
                 set mana (removeMana m pool) s
               _ -> s

      | mode /= SelectedAdvanced
      , not (null colors)
      ]
      ++
      [ topOpt s AskSelectedSideways "Turn sideways"
      $ setAndContinue
      $ set hand (handSelectMode SelectedSideways handVal)
      $ s
      | mode == SelectedBasic
      ]
      ++
      [ topOpt s (ActionButton "Play") "Play card"
        do playDeed mode deed
           updateThe_ hand (set handSelected Nothing)
           sync
           gameLoop
      ]
  where
  handVal   = view hand s
  pool      = view mana s


getPlayCardOptions :: TopInputOptions
getPlayCardOptions s =
  case view handSelected handVal of
    Just {} -> []
    Nothing -> [ ( playerId s :-> AskHand i
                 , "Select card"
                 , setAndContinue (set hand (handSelect i handVal) s)
                 )
               | i <- handPlayable handVal
               ]
  where
  handVal = view hand s


getManaPoolOptions :: TopInputOptions
getManaPoolOptions s =
  [ topOpt s (AskManaPool Gold) "Convert to base mana"
  $ askInputs "Convert to" (map cvtTo anyBasicMana)
  | hasMana 1 Gold manaPool
    -- XXX: and day time
  ]

  where
  manaPool = view mana s
  cvtTo b  =
    topOpt s (AskMana (BasicMana b)) "Convert to this color"
      $ setAndContinue
        $ set mana (convertMana Gold (BasicMana b) manaPool) s


getMoveOptions :: TopInputOptions
getMoveOptions s =
  [ topOpt s (AskLoc addr explo) lab
  $ setAndContinue
  $ over land
    (\la ->
    case explo of
      Nothing -> setPlayer addr la
      Just _  ->
        case exploreAt (addrGlobal addr) la of
          Ok (l1,newMons) -> l1
          Failed {} -> la
    ) s
  | (addr,explo) <- getPlayerNeighbours (view land s)
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

