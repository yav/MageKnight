module Spell (deeds, interactiveSpell) where

import Common
import Deed

-- XXX
deeds :: [Deed]
deeds = []
{-
  [ spellDeed Blue  "Chill"               "Lethal Chill" [] []
  , spellDeed Blue  "Mana Bolt"           "Mana Thunderbolt" [] []
  , spellDeed Blue  "Snow Storm"          "Blizzard" [] []
  , spellDeed Blue  "Space Bending"       "Time Bending" [] []
  , spellDeed Blue  "Mana Claim"          "Mana Curse" [] []
    -- interactive

  , spellDeed Green "Meditation"          "Trance" [] []
  , spellDeed Green "Restoration"         "Rebirth" [] []
  , spellDeed Green "Tremor"              "Earthquake" [] []
  , spellDeed Green "Underground Travel"  "Underground Attack" [] []
  , spellDeed Green "Energy Flow"         "Energy Steal" [] []
    -- interactive

  , spellDeed Red   "Burning Shield"      "Exploding Shield" [] []
  , spellDeed Red   "Demolish"            "Disintegrate" [] []
    -- only ignores fortifications due to the site, not the ones on the enemy.

  , spellDeed Red   "Fireball"            "Firestorm" [] []
  , spellDeed Red   "Flame Wall"          "Flame Wave" [] []
  , spellDeed Red   "Mana Meltdown"       "ManaRadiance" [] []
    -- interactive

  , spellDeed White "Call to Arms"        "Call to Glory" [] []
    -- "call to glory" may be used in a dungeon to hire a unit outside.
    -- "call to arms" may NOT be used in dungeon 
    -- "call to arms" and utem swordsmen:
    --   * The unit gets wounded IN THE OFFER
    --      (this seems equivalent to saying that can't be used again
    --       from the offer, until reqruited)
    --   * The unit is healed when recruited.
    -- Note: to implement these, we could keep a flag for units in the
    -- offer if "Call to arms" works on them.
    --  When entering a dungeon:
    --  1. save the "usable bit"
    --  2. Make all units in the offer not usable.
    --  3. restore the "usable bit"
    --  When using "call to glory" on utem swordsmen, set its "unusable" bit.
    -- ay, ay, ay.

  , spellDeed White "Expose"              "Mass Expose" [] []
  , spellDeed White "Whirlwind"           "Tornado" [] []
  , spellDeed White "Wings of Wind"       "Wings of Night" [] []
  , spellDeed White "Mind Read"           "Mind Steal" [] []
    -- interactive

  ]
-}

interactiveSpell :: Deed -> Bool
interactiveSpell d = deedName d `elem`
  [ "Mana Claim"
  , "Energy Flow"
  , "Mana Meltdown"
  , "Mind Read"
  ]


