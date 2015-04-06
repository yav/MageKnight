{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.Spell (deeds, interactiveSpell) where

import MageKnight.Common
import MageKnight.Deed

-- XXX
deeds :: [Deed]
deeds =
  [ spellDeed Blue "Chill"     "Lethal Chill" [] []
  , spellDeed Blue "Mana Bolt" "Mana Thunderbolt" [] []
  , spellDeed Blue "Mana Claim" "Mana Curse" [] []
  , spellDeed Blue "Snow Storm" "Blizzard" [] []
  , spellDeed Blue "Space Bending" "Time Bending" [] []

  , spellDeed Green "Energy Flow" "Energy Steal" [] []
  , spellDeed Green "Meditation" "Trance" [] []
  , spellDeed Green "Restoration" "Rebirth" [] []
  , spellDeed Green "Tremor" "Earthquake" [] []
  , spellDeed Green "Underground Travel" "Underground Attack" [] []

  , spellDeed Red "Burning Shield" "Exploding Shield" [] []
  , spellDeed Red "Demolish" "Disintegrate" [] []
  , spellDeed Red "Fireball" "Firestorm" [] []
  , spellDeed Red "Flame Wall" "Flame Wave" [] []
  , spellDeed Red "Mana Meltdown" "ManaRadiance" [] []

  , spellDeed White "Call to Arms" "Call to Glory" [] []
  , spellDeed White "Expose" "Mass Expose" [] []
  , spellDeed White "Mind Read" "Mind Steal" [] []
  , spellDeed White "Whirlwind" "Tornado" [] []
  , spellDeed White "Wings of Wind" "Wings of Night" [] []

  ]


interactiveSpell :: Deed -> Bool
interactiveSpell d = deedName d `elem`
  [ "Mana Claim"
  , "Energy Flow"
  , "Mana Meltdown"
  , "Mind Read"
  ]


