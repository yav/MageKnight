{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MageKnight.Action where

import MageKnight.Common
import MageKnight.Enemies
import MageKnight.Rule
import MageKnight.Deed
import MageKnight.Bag

import qualified Data.Set  as Set




basicDeck :: [ DeedName ]
basicDeck = [ "Stamina", "Stamina", "Determination", "Crystalize"
            , "March", "March", "Concentration", "Tranquility"
            , "Rage", "Rage", "Improvisation", "Threaten"
            , "Swiftness", "Swiftness", "Promise", "Mana Draw"
            ]

arytheaDeck :: [ DeedName ]
arytheaDeck = "Battle Versatility" : filter (/= "Improvisation") basicDeck

goldyxDeck :: [ DeedName ]
goldyxDeck = "Will Focus" : filter (/= "Concentration") basicDeck

tovakDeck :: [ DeedName ]
tovakDeck = "Cold Toughness" : filter (/= "Determination") basicDeck

norowasDeck :: [ DeedName ]
norowasDeck = "Noble manners" : filter (/= "Promise") basicDeck


actions :: [Deed] -> [Deed]
actions allDeeds =

  -- Blue

  [ actionDeed Blue "Stamina"
      [ [] --> replicate 2 Movement ]
      [ [] --> replicate 4 Movement ]

  , actionDeed Blue "Determination"
      [ [] --> replicate 2 (Attack Melee Physycal)
      , [] --> replicate 2 (Block Physycal)
      ]
      [ [] --> replicate 5 (Block Physycal) ]

  , actionDeed Blue "Crystalize"
      [ [ ManaToken (BasicMana b) ] --> [ ManaCrystal b ] | b <- anyBasicMana ]
      [ []                          --> [ ManaCrystal b ] | b <- anyBasicMana ]

  , actionDeed Blue "Cold Toughness"
      [ [] --> replicate 2 (Attack Melee Ice)
      , [] --> replicate 3 (Block Ice)
      ]
      [ [ Blocking name ] -->
          Blocking name : replicate (5 + coldToughnessBlockBonus e) (Block Ice)
        | e <- allEnemies, let name = enemyName e
      ]



  -- Green

  , actionDeed Green "Concentration"
      [ [] --> [ ManaToken (BasicMana b) ] | b <- [ Blue, White, Red ] ]
      [ r | c <- allDeeds, r <- concentrate 2 c ]

  , actionDeed Green "March"
      [ [] --> replicate 2 Movement ]
      [ [] --> replicate 4 Movement ]

  , actionDeed Green "Tranquility"
      [ [] --> [ Healing ]
      , [] --> [ DrawDeed ]
      ]
      [ [] --> replicate 2 Healing
      , [] --> replicate 2 DrawDeed
      ]

  , actionDeed Green "Will Focus"
      ( ( [] --> [ ManaCrystal Green ] )
      : [ [] --> [ ManaToken (BasicMana b) ] | b <- [ Blue, White, Red ] ])
      [ r | c <- allDeeds, r <- concentrate 3 c ]



  -- Red

  , actionDeed Red "Battle Versatility"
      [ [] --> replicate 2 (Attack Melee Physycal)
      , [] --> replicate 2 (Block Physycal)
      , [] --> [ Attack Ranged Physycal ]
      ]
      [ [] --> replicate 4 (Attack Melee Physycal)
      , [] --> replicate 4 (Block Physycal)
      , [] --> replicate 3 (Attack Melee Fire)
      , [] --> replicate 3 (Block Fire)
      , [] --> replicate 3 (Attack Ranged Physycal)
      , [] --> replicate 2 (Attack Siege Physycal)
      ]

  , actionDeed Red "Improvisation"
     (concatMap (improvise 3) allDeeds)
     (concatMap (improvise 5) allDeeds)

  , actionDeed Red "Rage"
    [ [] --> replicate 2 (Attack Melee Physycal)
    , [] --> replicate 2 (Block Physycal)
    ]
    [ [] --> replicate 4 (Attack Melee Physycal) ]

  , actionDeed Red "Threaten"
    [ [] --> replicate 2 Influence ]
    [ [] --> ReputationLoss : replicate 5 Influence ]


  -- White

  , actionDeed White "Mana Draw"
      [ [] --> [ ManaDie ] ]
      [ [ ManaSource m ] -->
                  (ManaSourceFixed n : replicate 3 (ManaToken n))
      | m <- anyMana, n <- filter (/= Gold) anyMana
      ]

  , actionDeed White "Noble Manners"
      [ [] --> FameGainIfInteract
             : replicate 2 Influence ]
      [ [] --> FameGainIfInteract
             : ReputationGainIfInteract
             : replicate 4 Influence ]

  , actionDeed White "Promise"
      [ [] --> replicate 2 Influence ]
      [ [] --> replicate 4 Influence ]

  , actionDeed White "Swiftness"
      [ [] --> replicate 2 Movement ]
      [ [] --> replicate 3 (Attack Ranged Physycal) ]

  ]




improvise :: Int -> Deed -> [ Rule ]
improvise amt Deed { .. } =
  [ [ ADeed deedName ] --> replicate amt act
  | act <- [ Movement, Influence, Attack Melee Physycal, Block Physycal]
  ]

concentrate :: Int -> Deed -> [ Rule ]
concentrate amt Deed { .. }
  | deedName `elem` [ "Concentration", "Will Focus" ] = []
  | otherwise =
    [ ADeed deedName : bagToList ruleIn -->
      flatGrouped [ (x, if affected x then a + amt else a)
                  | (x,a) <- bagToListGrouped ruleOut
                  ]
    | Rule { .. } <- deedPower
    ]
  where
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
                    AttcaksWith e _ | e /= Physycal -> 1
                    _                               -> 0

  fromAbilities = Set.size enemyAbilities



