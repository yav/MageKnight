{-# LANGUAGE Safe #-}
module MageKnight.Common where

import Data.Text ( Text )

data Element    = Physycal | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show)

type CardName = Text

data Resource =

    ManaToken Mana
  | ManaCrystal BasicMana

  | ManaSource Mana
  | ManaSourceFixed Mana    -- Not re-rolled
  | ManaDie

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element

  | Healing

  | ACard CardName

  | ReputationLoss

  | DrawCard

    deriving (Eq,Ord,Show)

data AttackType = Melee | Ranged | Siege
                  deriving (Eq,Ord,Show)


data BasicMana = Red | Green | Blue | White
                 deriving (Eq,Ord,Show)

data Mana = BasicMana BasicMana | Gold | Black
            deriving (Eq,Ord,Show)


anyBasicMana :: [ BasicMana ]
anyBasicMana = [ Red, Green, Blue, White ]

anyMana :: [ Mana ]
anyMana = Gold : Black : map BasicMana anyBasicMana

anyAttack :: [ AttackType ]
anyAttack = [ Melee, Ranged, Siege ]



