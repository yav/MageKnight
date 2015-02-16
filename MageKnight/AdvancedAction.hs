{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.AdvancedAction (deeds) where

import MageKnight.Common
import MageKnight.Deed
import MageKnight.Rule

deeds :: [Deed]
deeds =
  [
    -- blue
    actionDeed Blue "Crystal Mastery"
      [ [ManaCrystal b] --> replicate 2 (ManaCrystal b) | b <- anyBasicMana ]
      [ []              --> [ RegainUsedCrystals ] ]

  , actionDeed Blue "Frost Bridge"
      [ [] --> ChangeTerrainCost Swamp (DecreaseTo 1) : replicate 2 Movement ]
      [ [] --> ChangeTerrainCost Swamp (DecreaseTo 1)
             : ChangeTerrainCost Lake  (DecreaseTo 1)
             : replicate 4 Movement ]

  , actionDeed Blue "Ice Bolt"
      [ [] --> [ ManaCrystal Blue ] ]
      [ [] --> replicate 3 (Attack Ranged Ice) ]

  , actionDeed Blue "Ice Shield"
      [ [] --> replicate 3 (Block Ice) ]
      []    -- XXX: Ice shield

      -- XXX: Magic Talent
      -- XXX: Pure Magic
      -- XXX: Steady Tempo



    -- XXX: green
    -- XXX: red
    -- XXX: white
  ]

