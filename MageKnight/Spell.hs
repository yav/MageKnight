{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.Spell (deeds, interactiveSpell) where

import MageKnight.Deed

-- XXX
deeds :: [Deed]
deeds = []

interactiveSpell :: Deed -> Bool
interactiveSpell d = deedName d `elem`
  [ "Mana Claim"
  , "Energy Flow"
  , "Mana Meltdown"
  , "Mind Read"
  ]


