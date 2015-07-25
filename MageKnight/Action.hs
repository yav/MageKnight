{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
module MageKnight.Action
  ( blueSpecial
  , redSpecial
  , greenSpecial
  , whiteSpecial
  , deeds
  ) where

import MageKnight.Common
import MageKnight.Enemies
import MageKnight.Rule
import MageKnight.Deed

-- import {-# SOURCE #-} MageKnight.DeedDecks (allDeeds, actions)

import qualified Data.Set  as Set

allDeeds = undefined
actions = undefined

blueSpecial :: Deed
blueSpecial =
  actionDeed Blue "Cold Toughness"

    [ produces (2 *** Attack Melee Ice)
    , produces (3 *** Block Ice)
    ]

    [ onlyWhen (Blocking name) &&&
      produces ((5 + coldToughnessBlockBonus e) *** Block Ice)
      | e <- allEnemies, let name = enemyName e
    ]


greenSpecial :: Deed
greenSpecial =
  actionDeed Green "Will Focus"

    (   produces (ManaCrystal Green)
    : [ produces (ManaToken (BasicMana b)) | b <- [ Blue, White, Red ] ])

    [ r | c <- actions, r <- concentrate 3 c ]


redSpecial :: Deed
redSpecial =
  actionDeed Red "Battle Versatility"

    [ produces (2 *** Attack Melee Physycal)
    , produces (2 *** Block Physycal)
    , produces (1 *** Attack Ranged Physycal)
    ]

    [ produces (4 *** Attack Melee Physycal)
    , produces (4 *** Block Physycal)
    , produces (3 *** Attack Melee Fire)
    , produces (3 *** Block Fire)
    , produces (3 *** Attack Ranged Physycal)
    , produces (2 *** Attack Siege Physycal)
    ]

whiteSpecial :: Deed
whiteSpecial =
  actionDeed White "Noble Manners"
      [ produces (2 *** Influence) &&&
        produces (IfInteracted [ Fame ]) ]
      [ produces (4 *** Influence) &&&
        produces (IfInteracted [ Fame, Reputation ]) ]


deeds :: [Deed]
deeds =

  -- Blue

  [ actionDeed Blue "Stamina"
      [ produces (2 *** Movement) ]
      [ produces (4 *** Movement) ]

  , actionDeed Blue "Determination"
      [ produces (2 *** Attack Melee Physycal)
      , produces (2 *** Block Physycal)
      ]
      [ produces (5 *** Block Physycal) ]

  , actionDeed Blue "Crystallize"
      [ ManaToken (BasicMana b) --> ManaCrystal b | b <- anyBasicMana ]
      [ produces (ManaCrystal b) | b <- anyBasicMana ]



  -- Green

  , actionDeed Green "Concentration"
      [ produces (ManaToken (BasicMana b)) | b <- [ Blue, White, Red ] ]
      [ r | c <- actions, r <- concentrate 2 c ]

  , actionDeed Green "March"
      [ produces (2 *** Movement) ]
      [ produces (4 *** Movement) ]

  , actionDeed Green "Tranquility"
      [ produces Healing
      , produces DrawDeed
      ]
      [ produces (2 *** Healing)
      , produces (2 *** DrawDeed)
      ]



  -- Red

 , let nonWounds = filter (/= wound) allDeeds
   in actionDeed Red "Improvisation"
        (concatMap (improvise 3) nonWounds)
        (concatMap (improvise 5) nonWounds)

  , actionDeed Red "Rage"
    [ produces (2 *** Attack Melee Physycal)
    , produces (2 *** Block Physycal)
    ]
    [ produces (4 *** Attack Melee Physycal) ]

  , actionDeed Red "Threaten"
    [ produces (2 *** Influence) ]
    [ produces (5 *** Influence) &&& produces BadReputation ]


  -- White

  , actionDeed White "Mana Draw"
      [ produces ManaDie ]
      [ produces (ManaSourceFixed n) &&&
        ManaSource n --> 3 *** ManaToken n | n <- filter (/= Gold) anyMana
      ]

  , actionDeed White "Promise"
      [ produces (2 *** Influence) ]
      [ produces (4 *** Influence) ]

  , actionDeed White "Swiftness"
      [ produces (2 *** Movement) ]
      [ produces (3 *** Attack Ranged Physycal) ]

  ]




improvise :: Int -> Deed -> [ Rule ]
improvise amt Deed { .. } =
  [ DeedInHand deedName --> (amt, act)
  | act <- [ Movement, Influence, Attack Melee Physycal, Block Physycal]
  ]

concentrate :: Int -> Deed -> [ Rule ]
concentrate amt Deed { .. }
  | deedName `elem` [ "Concentration", "Will Focus" ] = []
  | otherwise =
    [ DeedInHand deedName --> map increase (ruleOutput r) | r <- deedPower ]
  where
  increase (n,r) = if affected r then (n + amt, r) else (n,r)

  affected a = case a of
                 Movement  -> True
                 Influence -> True
                 Attack {} -> True
                 Block {}  -> True
                 _         -> False

coldToughnessBlockBonus :: Enemy -> Int
coldToughnessBlockBonus Enemy  { .. } = fromAttack + fromAbilities
  where
  fromAttack    = case enemyAttack of
                    Summoner -> 0 -- Shouldn't be blocking this
                    AttacksWith e _ ->
                      case e of
                        Physycal -> 0
                        Fire     -> 1
                        Ice      -> 1
                        ColdFire -> 2

  fromAbilities = Set.size enemyAbilities



