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
  | Resists Element -- ^ "ColdFire" does not appear as a resiatnce
  | Swift
  | Brutal
  | Poisons
  | Paralyzes
    deriving (Show,Eq,Ord)

data EnemyAttack = AttacksWith Element Int
                 | Summoner
                   deriving (Eq,Show)

data EnemyType  = Orc | Guardian | Mage | Underworld | Citizen | Draconum
                  deriving (Eq,Ord,Show,Enum,Bounded)

allEnemyTypes :: [EnemyType]
allEnemyTypes = [ minBound .. maxBound ]

data Enemy = Enemy
  { enemyName       :: Text
  , enemyType       :: EnemyType
  , enemyArmor     :: Int
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
      , enemyArmor    = 3
      , enemyAttack    = AttacksWith Physycal 4
      , enemyFameGain  = 2
      , enemyAbilities = Set.empty
      }

  , replicate 2 Enemy
      { enemyName      = "Diggers"
      , enemyType      = Orc
      , enemyArmor    = 3
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = AttacksWith Physycal 3
      , enemyFameGain  = 2
      }

  , replicate 2 Enemy
      { enemyName      = "Cursed Hags"
      , enemyType      = Orc
      , enemyArmor    = 5
      , enemyAttack    = AttacksWith Physycal 3
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Wolf Riders"
      , enemyType      = Orc
      , enemyArmor    = 4
      , enemyAttack    = AttacksWith Physycal 3
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Ironclads"
      , enemyType      = Orc
      , enemyArmor    = 3
      , enemyAttack    = AttacksWith Physycal 4
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Orc Summoners"
      , enemyType      = Orc
      , enemyArmor    = 4
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
      , enemyArmor    = 4
      , enemyAttack    = AttacksWith Physycal 4
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Golems"
      , enemyType      = Guardian
      , enemyArmor    = 5
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = AttacksWith Physycal 2
      , enemyFameGain  = 4
      }

  , replicate 3 Enemy
      { enemyName      = "Guardsmen"
      , enemyType      = Guardian
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = AttacksWith Physycal 3
      , enemyFameGain  = 3
      }

  , replicate 2 Enemy
      { enemyName      = "Swordsmen"
      , enemyType      = Guardian
      , enemyArmor    = 5
      , enemyAbilities = Set.empty
      , enemyAttack    = AttacksWith Physycal 6
      , enemyFameGain  = 4
      }
  ]


dungeon :: [Enemy]
dungeon = concat

  [ replicate 2 Enemy
      { enemyName      = "Crypt Worm"
      , enemyType      = Underworld
      , enemyArmor    = 6
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = AttacksWith Physycal 6
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Gargoyle"
      , enemyType      = Underworld
      , enemyArmor    = 4
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = AttacksWith Physycal 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Medusa"
      , enemyType      = Underworld
      , enemyArmor    = 4
      , enemyAbilities = Set.fromList [ Paralyzes ]
      , enemyAttack    = AttacksWith Physycal 6
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Minotaur"
      , enemyType      = Underworld
      , enemyArmor    = 5
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyAttack    = AttacksWith Physycal 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Werewolf"
      , enemyType      = Underworld
      , enemyArmor    = 5
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyAttack    = AttacksWith Physycal 7
      , enemyFameGain  = 5
      }
  ]



magical :: [Enemy]
magical = concat
  [ replicate 2 Enemy
      { enemyName      = "Monks"
      , enemyType      = Mage
      , enemyArmor    = 5
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyAttack    = AttacksWith Physycal 5
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Illusionists"
      , enemyType      = Mage
      , enemyArmor    = 3
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = Summoner
      , enemyFameGain  = 4
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Mages"
      , enemyType      = Mage
      , enemyArmor    = 6
      , enemyAbilities = Set.fromList [ Resists Ice ]
      , enemyAttack    = AttacksWith Ice 5
      , enemyFameGain  = 5
      }

  , replicate 1 Enemy
      { enemyName      = "Ice Golems"
      , enemyType      = Mage
      , enemyArmor    = 4
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Ice,
                                                                Paralyzes ]
      , enemyAttack    = AttacksWith Ice 2
      , enemyFameGain  = 5
      }

  , replicate 2 Enemy
      { enemyName      = "Fire Mages"
      , enemyType      = Mage
      , enemyArmor    = 5
      , enemyAbilities = Set.fromList [ Resists Fire ]
      , enemyAttack    = AttacksWith Fire 6
      , enemyFameGain  = 5
      }

  , replicate 1 Enemy
      { enemyName      = "Fire Golems"
      , enemyType      = Mage
      , enemyArmor    = 4
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Fire, Brutal ]
      , enemyAttack    = AttacksWith Fire 3
      , enemyFameGain  = 5
      }

  ]


draconum :: [Enemy]
draconum = concat
  [ replicate 2 Enemy
      { enemyName      = "Fire Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Fire ]
      , enemyAttack    = AttacksWith Fire 9
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "High Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 9
      , enemyAbilities = Set.fromList [ Resists Fire, Resists Ice, Brutal ]
      , enemyAttack    = AttacksWith ColdFire 6
      , enemyFameGain  = 9
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Ice,
                                                              Paralyzes]
      , enemyAttack    = AttacksWith Ice 6
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "Swamp Dragon"
      , enemyType      = Draconum
      , enemyArmor    = 9
      , enemyAbilities = Set.fromList [ Swift, Poisons ]
      , enemyAttack    = AttacksWith Physycal 5
      , enemyFameGain  = 7
      }
  ]


citizens :: [Enemy]
citizens = concat
  [ replicate 2 Enemy
      { enemyName      = "Altem Guardsmen"
      , enemyType      = Citizen
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Fortified, Resists Fire, Resists Ice,
                                        Resists Physycal ]
      , enemyAttack    = AttacksWith Physycal 6
      , enemyFameGain  = 8
      }

  , replicate 2 Enemy
      { enemyName      = "Altem Mages"
      , enemyType      = Citizen
      , enemyArmor    = 8
      , enemyAbilities = Set.fromList [ Fortified, Resists Physycal,
                                                            Brutal, Poisons ]
      , enemyAttack    = AttacksWith ColdFire 4
      , enemyFameGain  = 8
      }

  , replicate 3 Enemy
      { enemyName      = "Freezers"
      , enemyType      = Citizen
      , enemyArmor    = 7
      , enemyAbilities = Set.fromList [ Resists Fire, Swift, Paralyzes ]
      , enemyAttack    = AttacksWith Ice 3
      , enemyFameGain  = 7
      }

  , replicate 3 Enemy
      { enemyName      = "Gunners"
      , enemyType      = Citizen
      , enemyArmor    = 6
      , enemyAbilities = Set.fromList [ Resists Ice, Brutal ]
      , enemyAttack    = AttacksWith Fire 6
      , enemyFameGain  = 7
      }

  ]
