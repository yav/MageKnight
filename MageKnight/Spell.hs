{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.Spell (deeds, interactiveSpell) where

import MageKnight.Common
import MageKnight.Deed

-- XXX
deeds :: [Deed]
deeds =
  [ -- Just to test layout
    spellDeed Red "Burning Shield" "Exploding Shield" [] []
  ]

interactiveSpell :: Deed -> Bool
interactiveSpell d = deedName d `elem`
  [ "Mana Claim"
  , "Energy Flow"
  , "Mana Meltdown"
  , "Mind Read"
  ]


