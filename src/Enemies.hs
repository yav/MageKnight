module Enemies
  ( Enemy(..)
  , EnemyType(..)
  , allEnemyTypes

  , EnemyAbility(..)
  , EnemyAttack(..)

  , allEnemies
  , orcs, guardians, mages, underworld, citizens, draconum
  ) where

import GHC.Generics(Generic)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Aeson(ToJSON)

import Common(Element(..))


data Enemy = Enemy
  { enemyName           :: Text
  , enemyType           :: EnemyType          -- ^ The color of the token
  , enemyArmor          :: Int                -- ^ How much armor
  , enemyAttack         :: [EnemyAttack]      -- ^ Attacks
  , enemyFameGain       :: Int                -- ^ Fame gain for kill
  , enemyReputationGain :: Int                -- ^ Reputation change for kill
  , enemyAbilities      :: Set EnemyAbility   -- ^ Special abilities
  } deriving (Show, Generic, ToJSON)

instance Eq Enemy where
  x == y = enemyName x == enemyName y

instance Ord Enemy where
  compare x y = compare (enemyName x) (enemyName y)


--------------------------------------------------------------------------------
data EnemyType  = Orc | Guardian | Mage | Underworld | Citizen | Draconum
                  deriving (Eq,Ord,Show,Enum,Bounded,Generic,ToJSON)

allEnemyTypes :: [EnemyType]
allEnemyTypes = [ minBound .. maxBound ]
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data EnemyAbility =

    -- attack control
    Fortified       -- ^ No ranged attack
  | Unfortified     -- ^ Never fortified

    -- block
  | Elusive Int     -- ^ Higher defence, unless fully blocked
  | Resists Element -- ^ "ColdFire" does not appear as a separate resiatnce;
                    --   instead, an enemy would have fire and ice reistance.
  | ResistArcane    -- ^ Can only be attacked/block no special stuff
  | Cumbersome      -- ^ May use move as block
  | Swift           -- ^ Needs twice the usual block

    -- damage
  | Brutal          -- ^ Deals twice the usual damage
  | Poisons         -- ^ Get twice the wounds. Hero: half in discard pile.
  | Paralyzes       -- ^ If wounded:
                    -- Hero: discard non-wound cards;  Unit: destroyed.
  | Assassin        -- ^ Damage goes to hero
    deriving (Show,Eq,Ord,Generic,ToJSON)

data EnemyAttack = AttacksWith Element Int
                 | Summoner
                   deriving (Eq,Show,Generic,ToJSON)
--------------------------------------------------------------------------------


allEnemies :: [Enemy]
allEnemies = orcs ++ guardians ++ underworld ++ mages ++ draconum ++ citizens


orcs :: [Enemy]
orcs = concat

  [ replicate 2 Enemy
      { enemyName      = "Prowlers"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAbilities = Set.empty
      , enemyAttack    = [AttacksWith Physycal 4]
      , enemyFameGain  = 2
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Diggers"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = [AttacksWith Physycal 3]
      , enemyFameGain  = 2
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Cursed Hags"
      , enemyType      = Orc
      , enemyArmor     = 5
      , enemyAttack    = [AttacksWith Physycal 3]
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Wolf Riders"
      , enemyType      = Orc
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyAttack    = [ AttacksWith Physycal 3 ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Ironclads"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyAttack    = [AttacksWith Physycal 4]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Orc Summoners"
      , enemyType      = Orc
      , enemyArmor     = 4
      , enemyAbilities = Set.empty
      , enemyAttack    = [Summoner]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Orc Skirmishers"
      , enemyType      = Orc
      , enemyArmor     = 4
      , enemyAbilities = Set.empty
      , enemyAttack    = replicate 2 (AttacksWith Physycal 1)
      , enemyFameGain  = 2
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Orc Trackers"
      , enemyType      = Orc
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Elusive 6, Assassin ]
      , enemyAttack    = [ AttacksWith Physycal 4 ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Orc War Beasts"
      , enemyType      = Orc
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList
                           [ Unfortified, Resists Fire, Resists Ice, Brutal ]
      , enemyAttack    = [ AttacksWith Physycal 3 ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Orc Stonethrower"
      , enemyType      = Orc
      , enemyArmor     = 2
      , enemyAbilities = Set.fromList
                          [ Fortified, Resists Physycal, Cumbersome ]
      , enemyAttack    = [ AttacksWith Physycal 7 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }



  ]


guardians :: [Enemy]
guardians = concat

  [ replicate 3 Enemy
      { enemyName      = "Crossbowmen"
      , enemyType      = Guardian
      , enemyArmor     = 4
      , enemyAttack    = [ AttacksWith Physycal 4 ]
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }

  , replicate 3 Enemy
      { enemyName      = "Guardsmen"
      , enemyType      = Guardian
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = [ AttacksWith Physycal 3 ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Swordsmen"
      , enemyType      = Guardian
      , enemyArmor     = 5
      , enemyAbilities = Set.empty
      , enemyAttack    = [ AttacksWith Physycal 6 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Golems"
      , enemyType      = Guardian
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = [ AttacksWith Physycal 2 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

   , replicate 1 Enemy
      { enemyName      = "Ice Heroes"
      , enemyType      = Guardian
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Resists Ice ]
      , enemyAttack    = [ AttacksWith Physycal 3, AttacksWith Ice 3 ]
      , enemyFameGain  = 5
      , enemyReputationGain = -1
      }

   , replicate 1 Enemy
      { enemyName      = "Fire Heroes"
      , enemyType      = Guardian
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Resists Fire ]
      , enemyAttack    = [ AttacksWith Physycal 4, AttacksWith Fire 2 ]
      , enemyFameGain  = 5
      , enemyReputationGain = -1
      }

   , replicate 1 Enemy
      { enemyName      = "Swift Heroes"
      , enemyType      = Guardian
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyAttack    = [ AttacksWith Physycal 3, AttacksWith Physycal 2 ]
      , enemyFameGain  = 5
      , enemyReputationGain = -1
      }

   , replicate 1 Enemy
      { enemyName      = "Fortified Heroes"
      , enemyType      = Guardian
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = [ AttacksWith Physycal 5, AttacksWith Physycal 3 ]
      , enemyFameGain  = 5
      , enemyReputationGain = -1
      }

   , replicate 2 Enemy
      { enemyName      = "Thugs"
      , enemyType      = Guardian
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyAttack    = [ AttacksWith Physycal 3 ]
      , enemyFameGain  = 2
      , enemyReputationGain = 1
      }

   , replicate 2 Enemy
      { enemyName      = "Shocktroops"
      , enemyType      = Guardian
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Unfortified, Elusive 6 ]
      , enemyAttack    = [ AttacksWith Physycal 5 ]
      , enemyFameGain  = 3
      , enemyReputationGain = 0
      }





  ]


underworld :: [Enemy]
underworld = concat

  [ replicate 2 Enemy
      { enemyName      = "Crypt Worm"
      , enemyType      = Underworld
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Fortified ]
      , enemyAttack    = [ AttacksWith Physycal 6 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Gargoyle"
      , enemyType      = Underworld
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = [ AttacksWith Physycal 5 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Medusa"
      , enemyType      = Underworld
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Paralyzes ]
      , enemyAttack    = [ AttacksWith Physycal 6 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Minotaur"
      , enemyType      = Underworld
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Brutal ]
      , enemyAttack    = [ AttacksWith Physycal 5 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Werewolf"
      , enemyType      = Underworld
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Swift ]
      , enemyAttack    = [ AttacksWith Physycal 7 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Shadow"
      , enemyType      = Underworld
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Elusive 8, ResistArcane ]
      , enemyAttack    = [ AttacksWith ColdFire 4 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Manticore"
      , enemyType      = Underworld
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Resists Fire, Swift, Assassin, Poisons ]
      , enemyAttack    = [ AttacksWith Physycal 4 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Hydra"
      , enemyType      = Underworld
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Resists Ice ]
      , enemyAttack    = replicate 3 (AttacksWith Physycal 2)
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }
  ]



mages :: [Enemy]
mages = concat
  [ replicate 2 Enemy
      { enemyName      = "Monks"
      , enemyType      = Mage
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Poisons ]
      , enemyAttack    = [ AttacksWith Physycal 5 ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Illusionists"
      , enemyType      = Mage
      , enemyArmor     = 3
      , enemyAbilities = Set.fromList [ Resists Physycal ]
      , enemyAttack    = [ Summoner ]
      , enemyFameGain  = 4
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Mages"
      , enemyType      = Mage
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Resists Ice ]
      , enemyAttack    = [ AttacksWith Ice 5 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 1 Enemy
      { enemyName      = "Ice Golems"
      , enemyType      = Mage
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Ice,
                                                                Paralyzes ]
      , enemyAttack    = [ AttacksWith Ice 2 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Fire Mages"
      , enemyType      = Mage
      , enemyArmor     = 5
      , enemyAbilities = Set.fromList [ Resists Fire ]
      , enemyAttack    = [ AttacksWith Fire 6 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 1 Enemy
      { enemyName      = "Fire Golems"
      , enemyType      = Mage
      , enemyArmor     = 4
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Fire, Brutal ]
      , enemyAttack    = [ AttacksWith Fire 3 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Sorcerers"
      , enemyType      = Mage
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ ResistArcane, Assassin, Poisons ]
      , enemyAttack    = [ AttacksWith Physycal 6 ]
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Magic Familiars"
      , enemyType      = Mage
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Unfortified, Brutal ]
      , enemyAttack    = replicate 2 (AttacksWith Physycal 3)
      , enemyFameGain  = 5
      , enemyReputationGain = 0
      }



  ]


draconum :: [Enemy]
draconum = concat
  [ replicate 2 Enemy
      { enemyName      = "Fire Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Fire ]
      , enemyAttack    = [ AttacksWith Fire 9 ]
      , enemyFameGain  = 8
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "High Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 9
      , enemyAbilities = Set.fromList [ Resists Fire, Resists Ice, Brutal ]
      , enemyAttack    = [ AttacksWith ColdFire 6 ]
      , enemyFameGain  = 9
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Ice Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Resists Physycal, Resists Ice,
                                                              Paralyzes]
      , enemyAttack    = [ AttacksWith Ice 6 ]
      , enemyFameGain  = 8
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Swamp Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 9
      , enemyAbilities = Set.fromList [ Swift, Poisons ]
      , enemyAttack    = [ AttacksWith Physycal 5 ]
      , enemyFameGain  = 7
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Storm Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Elusive 14, Resists Ice, Swift ]
      , enemyAttack    = [ AttacksWith Ice 4 ]
      , enemyFameGain  = 7
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Lava Dragon"
      , enemyType      = Draconum
      , enemyArmor     = 8
      , enemyAbilities = Set.fromList [ Fortified, Resists Fire, Brutal ]
      , enemyAttack    = [ AttacksWith Fire 6 ]
      , enemyFameGain  = 8
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Dragon Summoner"
      , enemyType      = Draconum
      , enemyArmor     = 8
      , enemyAbilities = Set.fromList [ Resists Physycal, ResistArcane ]
      , enemyAttack    = replicate 2 Summoner
      , enemyFameGain  = 9
      , enemyReputationGain = 0
      }



  ]


citizens :: [Enemy]
citizens = concat
  [ replicate 2 Enemy
      { enemyName      = "Altem Guardsmen"
      , enemyType      = Citizen
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Fortified, Resists Fire, Resists Ice,
                                        Resists Physycal ]
      , enemyAttack    = [ AttacksWith Physycal 6 ]
      , enemyFameGain  = 8
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Altem Mages"
      , enemyType      = Citizen
      , enemyArmor     = 8
      , enemyAbilities = Set.fromList [ Fortified, Resists Physycal,
                                                            Brutal, Poisons ]
      , enemyAttack    = [ AttacksWith ColdFire 4 ]
      , enemyFameGain  = 8
      , enemyReputationGain = 0
      }

  , replicate 3 Enemy
      { enemyName      = "Freezers"
      , enemyType      = Citizen
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Resists Fire, Swift, Paralyzes ]
      , enemyAttack    = [ AttacksWith Ice 3 ]
      , enemyFameGain  = 7
      , enemyReputationGain = 0
      }

  , replicate 3 Enemy
      { enemyName      = "Gunners"
      , enemyType      = Citizen
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Resists Ice, Brutal ]
      , enemyAttack    = [ AttacksWith Fire 6 ]
      , enemyFameGain  = 7
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Grim Legionnaires"
      , enemyType      = Citizen
      , enemyArmor     = 10
      , enemyAbilities = Set.fromList [ Unfortified, ResistArcane ]
      , enemyAttack    = [ AttacksWith Physycal 11 ]
      , enemyFameGain  = 8
      , enemyReputationGain = 0
      }

  , replicate 2 Enemy
      { enemyName      = "Delphana Masters"
      , enemyType      = Citizen
      , enemyArmor     = 8
      , enemyAbilities = Set.fromList
                           [ Resists Fire, Resists Ice, Assassin, Paralyzes ]
      , enemyAttack    = [ AttacksWith ColdFire 5 ]
      , enemyFameGain  = 9
      , enemyReputationGain = 0
      }

  , replicate 1 Enemy
      { enemyName      = "Fire Catapult"
      , enemyType      = Citizen
      , enemyArmor     = 7
      , enemyAbilities = Set.fromList [ Fortified, Cumbersome ]
      , enemyAttack    = [ AttacksWith Fire 8 ]
      , enemyFameGain  = 7
      , enemyReputationGain = 0
      }

  , replicate 1 Enemy
      { enemyName      = "Ice Catapult"
      , enemyType      = Citizen
      , enemyArmor     = 6
      , enemyAbilities = Set.fromList [ Fortified, Cumbersome ]
      , enemyAttack    = [ AttacksWith Ice 9 ]
      , enemyFameGain  = 7
      , enemyReputationGain = 0
      }

  ]
