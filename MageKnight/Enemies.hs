{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module MageKnight.Enemies where

import MageKnight.Common(Element(..))

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)


data EnemyAbility =

    Fortified
  | Resists Element
  | Swift
  | Brutal
  | Poisons
  | Paralyzes
    deriving (Show,Eq,Ord)

data EnemyAttack = AttcaksWith Element Int
                 | Summoner
                   deriving (Eq,Show)

data EnemyType  = Orc | Guardian | Mage | Underworld | Citizen | Draconum
                  deriving (Eq,Show)

data Enemy = Enemy
  { enemyName       :: Text
  , enemyType       :: EnemyType
  , enemyArmour     :: Int
  , enemyAttack     :: EnemyAttack
  , enemyFameGain   :: Int
  , enemyAbilities  :: Set EnemyAbility
  }


allEnemies :: [Enemy]
allEnemies = orcs ++ keep ++ dungeon


orcs :: [Enemy]
orcs = concat

  [ replicate 2 Enemy
      { enemyName      = "Prowlers"
      , enemyType      = Orc
      , enemyArmour    = 3
      , enemyAttack    = AttcaksWith Physycal 4
      , enemyFameGain  = 2
      , enemyAbilities = Set.empty
      }

  , replicate 2 Enemy
      { enemyName      = "Diggers"
      , enemyType      = Orc
      , enemyArmour    = 3
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = AttcaksWith Physycal 3
      , enemyFameGain  = 2
      }

  , replicate 2 Enemy
      { enemyName      = "Cursed Hags"
      , enemyType      = Orc
      , enemyArmour    = 5
      , enemyAttack    = AttcaksWith Physycal 3
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Wolf Riders"
      , enemyType      = Orc
      , enemyArmour    = 4
      , enemyAttack    = AttcaksWith Physycal 3
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Ironclads"
      , enemyType      = Orc
      , enemyArmour    = 3
      , enemyAttack    = AttcaksWith Physycal 4
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Orc Summoners"
      , enemyType      = Orc
      , enemyArmour    = 4
      , enemyAttack    = Summoner
      , enemyFameGain  = 4
      , enemyAbilities = Set.empty
      }
  ]


keep :: [Enemy]
keep = concat

  [ replicate 3 Enemy
      { enemyName      = "Crossbowmen"
      , enemyType      = Guardian
      , enemyArmour    = 4
      , enemyAttack    = AttcaksWith Physycal 4
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Golems"
      , enemyType      = Guardian
      , enemyArmour    = 5
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = AttcaksWith Physycal 2
      , enemyFameGain  = 4
      }

  , replicate 3 Enemy
      { enemyName      = "Guardsmen"
      , enemyType      = Guardian
      , enemyArmour    = 7
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = AttcaksWith Physycal 3
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Swordsmen"
      , enemyType      = Guardian
      , enemyArmour    = 5
      , enemyAbilities = Set.empty
      , enemyAttack    = AttcaksWith Physycal 6
      , enemyFameGain  = 4
      }
  ]


dungeon :: [Enemy]
dungeon = concat

  [ replicate 2 Enemy
      { enemyName      = "Crypt Worm"
      , enemyType      = Underworld
      , enemyArmour    = 6
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = AttcaksWith Physycal 6
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Gargoyle"
      , enemyType      = Underworld
      , enemyArmour    = 4
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = AttcaksWith Physycal 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Medusa"
      , enemyType      = Underworld
      , enemyArmour    = 4
      , enemyAbilities = Set.fromList [ Paralyzes ]
      , enemyAttack    = AttcaksWith Physycal 6
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Minoutaur"
      , enemyType      = Underworld
      , enemyArmour    = 5
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyAttack    = AttcaksWith Physycal 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Werewolf"
      , enemyType      = Underworld
      , enemyArmour    = 5
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyAttack    = AttcaksWith Physycal 7
      , enemyFameGain  = 5
      }
  ]


