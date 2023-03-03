module AdvancedAction (deeds) where

import Common
import Deed
import Rule

import {-# SOURCE #-} DeedDecks (actions)


deeds :: [Deed]
deeds = blueDeeds ++ greenDeeds ++ redDeeds ++ whiteDeeds

blueDeeds :: [Deed]
blueDeeds = []
{-
  [ deed "Crystal Mastery"
      [ ManaCrystal b --> 2 *** ManaCrystal b | b <- anyBasicMana ]
      [ produces RegainUsedCrystals ]

  , deed "Frost Bridge"
      [ produces (ChangeTerrainCost Swamp (DecreaseTo 1)) &&&
        produces (2 *** Movement) ]
      [ produces (ChangeTerrainCost Swamp (DecreaseTo 1)) &&&
        produces (ChangeTerrainCost Lake  (DecreaseTo 1)) &&&
        produces (4 *** Movement) ]

  , deed "Ice Bolt"
      [ produces (ManaCrystal Blue) ]
      [ produces (3 *** Attack Ranged Ice) ]

  , deed "Ice Shield"
      [ produces (3 *** Block Ice) ]
      [ produces (3 *** Block Ice) &&&
        produces (IfNotResist Ice (replicate 3 DecreaseArmor))
      ]


  , let gain n x y = ManaToken (BasicMana x) --> n *** y
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
      [ produces (2 *** Movement) &&& produces (ToDeedDeckBottom name) ]
      [ produces (4 *** Movement) &&& produces (ToDeedDeckTop name) ]

  , deed "Magic Talent" [] [] -- XXX
  ]

  where
  deed = advancedActionDeed Blue
-}

greenDeeds :: [Deed]
greenDeeds = []
{-
  [ deed "Crushing Bolt"
      [ produces (ManaCrystal Green) ]
      [ produces (3 *** Attack Siege Physycal) ]

  , deed "Refreshing Walk"
      [ produces (2 *** Movement) &&& produces Healing ]
      [ produces (4 *** Movement) &&& produces (2 *** Healing) ]

  , deed "Regeneration"
      [ produces Healing &&& produces (ReadyUnit l) | l <- [1,2] ]
      [ produces (2 *** Healing) &&& produces (ReadyUnit l) | l <- [1,2,3] ]

  , let terrain c t xs = produces (ChangeTerrainCost t c) &&& xs
        ts             = [ Hills, Forest, Wasteland, Desert, Swamp ]
    in deed "Path Finding"
        [ foldr (terrain (DecreaseBy 1 2)) (produces (2 *** Movement)) ts ]
        [ foldr (terrain (DecreaseTo 2))   (produces (4 *** Movement)) ts ]

  , deed "Ambush"
      [ produces (2 *** Movement) &&& produces Ambush ]
      [ produces (4 *** Movement) &&& produces PowerAmbush ]

  , deed "In Need" [] [] -- XXX
  , deed "Training" [] [] -- XXX
  ]
  where
  deed = advancedActionDeed Green
-}

redDeeds :: [Deed]
redDeeds = []
{-
  [ deed "Blood Rage"
      [ produces (2 *** Attack Melee Physycal)
      , produces (5 *** Attack Melee Physycal) &&& produces GainWound ]
      [ produces (4 *** Attack Melee Physycal)
      , produces (9 *** Attack Melee Physycal) &&& produces GainWound ]

  , deed "Blood Ritual"
      [ produces GainWound &&& produces (ManaToken (BasicMana Red))
          &&& produces (ManaToken t) | t <- anyMana ]
      [ produces GainWound &&& produces (ManaToken t1) &&&
        produces (ManaToken t2) &&& produces (ManaToken t3)
          | t1 <- anyMana, t2 <- anyMana, t3 <- anyMana ]

  , deed "Fire Bolt"
      [ produces (ManaCrystal Red) ]
      [ produces (3 *** Attack Ranged Fire) ]

  , deed "Intimidate"
      [ produces p &&& produces BadReputation
          | p <- [ 4 *** Influence, 3 *** Attack Melee Physycal ] ]

      [ produces p &&& produces (2 *** BadReputation)
          | p <- [ 8 *** Influence, 7 *** Attack Melee Physycal ] ]

  , deed "Decompose"
      [ DeedInHand n --> DeedDestroyed n &&& produces (2 *** ManaCrystal c)
          | a <- actions, let n = deedName a, n /= "Decompose"
          , Just c <- [ deedColor a ]
      ]
      [ DeedInHand n --> DeedDestroyed n :
                        [ ManaCrystal b | b <- anyBasicMana, b /= c ]
          | a <- actions, let n = deedName a, n /= "Decompose"
          , Just c <- [ deedColor a ]
      ]


  , deed "Maximal Effect" [] []
  , deed "Into the Heat" [] [] -- XXX

  ]
  where
  deed = advancedActionDeed Red
-}

-- XXX: white
whiteDeeds :: [Deed]
whiteDeeds = []
{-
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
-}
