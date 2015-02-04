{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.Enemies where

import MageKnight.Common(Element(..))
import MageKnight.ResourceQ(ResourceQ)
import MageKnight.JSON

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import           Data.Text (Text)

type EnemyPool = Map EnemyType (ResourceQ Enemy)

data EnemyAbility =

    Fortified
  | FortifiedX2
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
                  deriving (Eq,Ord,Show,Enum,Bounded)

allEnemyTypes :: [EnemyType]
allEnemyTypes = [ minBound .. maxBound ]

data Enemy = Enemy
  { enemyName       :: Text
  , enemyType       :: EnemyType
  , enemyArmour     :: Int
  , enemyAttack     :: EnemyAttack
  , enemyFameGain   :: Int
  , enemyAbilities  :: Set EnemyAbility
  } deriving Show

instance Eq Enemy where
  x == y = enemyName x == enemyName y

instance Ord Enemy where
  compare x y = compare (enemyName x) (enemyName y)



instance Export EnemyType where
  toJS et = toJS (txt :: Text)
    where
    txt = case et of
            Orc         -> "orc"
            Guardian    -> "guardian"
            Mage        -> "mage"
            Underworld  -> "underworld"
            Citizen     -> "citizen"
            Draconum    -> "draconum"



--------------------------------------------------------------------------------

allEnemies :: [Enemy]
allEnemies = orcs ++ keep ++ dungeon ++ magical ++ draconum ++ citizens


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
      { enemyName      = "Minotaur"
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



magical :: [Enemy]
magical = concat
  [ replicate 2 Enemy
      { enemyName      = "Monks"
      , enemyType      = Mage
      , enemyArmour    = 5
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyAttack    = AttcaksWith Physycal 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Illusionists"
      , enemyType      = Mage
      , enemyArmour    = 3
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = Summoner
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Mages"
      , enemyType      = Mage
      , enemyArmour    = 6
      , enemyAbilities = Set.fromList [ Resists Ice ]
      , enemyAttack    = AttcaksWith Ice 5
      , enemyFameGain  = 5
      }

  , replicate 1 Enemy
      { enemyName      = "Ice Golems"
      , enemyType      = Mage
      , enemyArmour    = 4
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Ice,
                                                                Paralyzes ]
      , enemyAttack    = AttcaksWith Ice 2
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Fire Mages"
      , enemyType      = Mage
      , enemyArmour    = 5
      , enemyAbilities = Set.fromList [ Resists Fire ]
      , enemyAttack    = AttcaksWith Fire 6
      , enemyFameGain  = 5
      }

  , replicate 1 Enemy
      { enemyName      = "Fire Golems"
      , enemyType      = Mage
      , enemyArmour    = 4
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Fire, Brutal ]
      , enemyAttack    = AttcaksWith Fire 3
      , enemyFameGain  = 5
      }

  ]


draconum :: [Enemy]
draconum = concat
  [ replicate 2 Enemy
      { enemyName      = "Fire Dragon"
      , enemyType      = Draconum
      , enemyArmour    = 7
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Fire ]
      , enemyAttack    = AttcaksWith Fire 9
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "High Dragon"
      , enemyType      = Draconum
      , enemyArmour    = 9
      , enemyAbilities = Set.fromList [ Resists Fire, Resists Ice, Brutal ]
      , enemyAttack    = AttcaksWith ColdFire 6
      , enemyFameGain  = 9
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Dragon"
      , enemyType      = Draconum
      , enemyArmour    = 7
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Ice,
                                                              Paralyzes]
      , enemyAttack    = AttcaksWith Ice 6
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "Swamp Dragon"
      , enemyType      = Draconum
      , enemyArmour    = 9
      , enemyAbilities = Set.fromList [ Swift, Poisons ]
      , enemyAttack    = AttcaksWith Physycal 5
      , enemyFameGain  = 7
      }
  ]


citizens :: [Enemy]
citizens = concat
  [ replicate 2 Enemy
      { enemyName      = "Altem Guardsmen"
      , enemyType      = Citizen
      , enemyArmour    = 7
      , enemyAbilities = Set.fromList [ Fortified, Resists Fire, Resists Ice,
                                        Resists Physycal ]
      , enemyAttack    = AttcaksWith Physycal 6
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "Altem Mages"
      , enemyType      = Citizen
      , enemyArmour    = 8
      , enemyAbilities = Set.fromList [ Fortified, Resists Physycal,
                                                            Brutal, Poisons ]
      , enemyAttack    = AttcaksWith ColdFire 4
      , enemyFameGain  = 8
      }

  , replicate 3 Enemy
      { enemyName      = "Freezers"
      , enemyType      = Citizen
      , enemyArmour    = 7
      , enemyAbilities = Set.fromList [ Resists Fire, Swift, Paralyzes ]
      , enemyAttack    = AttcaksWith Ice 3
      , enemyFameGain  = 7
      }

  , replicate 3 Enemy
      { enemyName      = "Gunners"
      , enemyType      = Citizen
      , enemyArmour    = 6
      , enemyAbilities = Set.fromList [ Resists Ice, Brutal ]
      , enemyAttack    = AttcaksWith Fire 6
      , enemyFameGain  = 7
      }

  ]
