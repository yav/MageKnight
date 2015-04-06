{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.AdvancedAction (deeds) where

import MageKnight.Common
import MageKnight.Deed
import MageKnight.Rule


deeds :: [Deed]
deeds = blueDeeds ++ greenDeeds ++ redDeeds ++ whiteDeeds

blueDeeds :: [Deed]
blueDeeds =
  [ deed "Crystal Mastery"
      [ [ManaCrystal b] --> replicate 2 (ManaCrystal b) | b <- anyBasicMana ]
      [ []              --> [ RegainUsedCrystals ] ]

  , deed "Frost Bridge"
      [ [] --> ChangeTerrainCost Swamp (DecreaseTo 1) : replicate 2 Movement ]
      [ [] --> ChangeTerrainCost Swamp (DecreaseTo 1)
             : ChangeTerrainCost Lake  (DecreaseTo 1)
             : replicate 4 Movement ]

  , deed "Ice Bolt"
      [ [] --> [ ManaCrystal Blue ] ]
      [ [] --> replicate 3 (Attack Ranged Ice) ]

  , deed "Ice Shield"
      [ [] --> replicate 3 (Block Ice) ]
      []    -- XXX: Ice shield


  -- XXX: Magic Talent
  , deed "Magic Talent" [] []

  , let gain n x y = [ManaToken (BasicMana x)] --> replicate n y
        table      = [ (Green, Movement)
                     , (White, Influence)
                     , (Blue,  Block Physycal)
                     , (Red,   Attack Melee Physycal)
                     ]

    in deed "Pure Magic"
      [ gain 4 mana reward | (mana,reward) <- table ]
      [ gain 7 mana reward | (mana,reward) <- table ]

  , let name = "Steady Tempo"
    in deed name
      [ [] --> ToDeedDeckBottom name : replicate 2 Movement ]
      [ [] --> ToDeedDeckTop name    : replicate 4 Movement ]

  ]

  where
  deed = advancedActionDeed Blue

-- XXX: green
greenDeeds :: [Deed]
greenDeeds =
  [ deed "Crushing Bolt"
      [ [] --> [ ManaCrystal Green ] ]
      [ [] --> replicate 3 (Attack Siege Physycal) ]

  , deed "Ambush" [] []
  , deed "In Need" [] []
  , deed "Path Finding" [] []
  , deed "Refreshing Walk" [] []
  , deed "Regeneration" [] []
  , deed "Training" [] []
  ]
  where
  deed = advancedActionDeed Green

-- XXX: red
redDeeds :: [Deed]
redDeeds =
  [ deed "Blood Rage" [] []
  , deed "Blood Ritual" [] []
  , deed "Decompose" [] []
  , deed "Fire Bolt" [] []
  , deed "Intimidate" [] []
  , deed "Into the Heat" [] []
  , deed "Maximal Effect" [] []
  ]
  where
  deed = advancedActionDeed Red

-- XXX: white
whiteDeeds :: [Deed]
whiteDeeds =
  [ deed "Agility" [] []
  , deed "Diplomacy" [] []
  , deed "Heroic Tale" [] []
  , deed "Learning" [] []
  , deed "Mana Storm" [] []
  , deed "Song of Wind" [] []
  , deed "Swift Bolt" [] []
  ]
  where
  deed = advancedActionDeed White
