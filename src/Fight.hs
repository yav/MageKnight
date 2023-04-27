module Fight where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set

import KOI.Bag

import Enemies

type SiteId     = Int
type GroupId    = Int
type EnemyId    = Int

-- | The overall state of the battle
data Battle = Battle
  { battleEnemies        :: Map EnemyId ActiveEnemy
  , battleFortifiedSites :: Set SiteId
  , battlePhase          :: Phase
  }

--  | A single participant in a battle
data ActiveEnemy = ActiveEnemy
  { enemyToken          :: Enemy         -- ^ Original enemy stats  
  , enemySite           :: SiteId        -- ^ Where we reside
  , enemyAlive          :: Bool          -- ^ Has this enemy been killed
  , enemyIsSummoned     :: Maybe EnemyId -- ^ Wo did we get summoned by
  , enemyAttacks        :: Bool          -- ^ Should this enemy attack
  }

enemyFortifications :: Battle -> ActiveEnemy -> Int
enemyFortifications btl en
  | Unfortified `Set.member` abilities = 0
  | otherwise                          = fromSite + fromAbility
  where
  abilities = enemyAbilities (enemyToken en)

  fromSite
    | enemySite en `Set.member` battleFortifiedSites btl = 1
    | otherwise                                          = 0

  fromAbility
    | Fortified `Set.member` abilities = 1
    | otherwise                        = 0

data Phase =
    Group   GroupingPhase
  | Attack  AttackPhase
  | Block   BlockPhase
  | Damage  DamagePhase

-- | The enemy currently being blocked
data Blocking = Blocking
  { blocking :: ActiveEnemy
  -- + block
  }


type AttackId = Int

-- | Blocking phase
data BlockPhase = BlockPhase
  { unblockedAttacks    :: Set (EnemyId, AttackId)
    -- ^ Enemy attacks that need blocking.  Includes attacks from summons.

  , currentlyBlocking   :: Maybe Blocking
  }

data Wound = Wound
  { isPoison, isParalyzing, isAssasination :: Bool
  } deriving (Eq,Ord)

data DamagePhase = DamagePhase
  { unassigned :: Bag Wound
  }


--------------------------------------------------------------------------------

data BattleSite = BattleSite
  { siteFortified :: Bool
  , siteEnemies   :: [Enemy]
  }

-- | Start a battle
startBattle :: [BattleSite] -> Battle
startBattle sites =
  Battle
    { battleEnemies        = allBattleEnemies
    , battleFortifiedSites = allFortifiedSites
    , battlePhase          = startPhase
    }
  where
  (_,allBattleEnemies,allFortifiedSites) =
    foldr addSite (0,Map.empty,Set.empty) sites

  startPhase =
    let g = startGroupPhase (Map.keysSet allBattleEnemies)
    in if groupPhaseDone g
        then Attack (startAttackPhase True (groupPhaseGroups g))
        else Group g

  addSite site (nextSite,enemies,fortifiedSites) =
    ( nextSite + 1
    , foldr (addEnemy nextSite) enemies (siteEnemies site)
    , if siteFortified site
        then Set.insert nextSite fortifiedSites
        else fortifiedSites
    )

  addEnemy siteId enemy enemies =
    Map.insert (Map.size enemies) (activateEnemy siteId enemy) enemies

  activateEnemy siteId enemy =
    ActiveEnemy
      { enemyToken = enemy
      , enemySite = siteId
      , enemyAlive = True
      , enemyIsSummoned = Nothing
      , enemyAttacks = True
      }

--------------------------------------------------------------------------------
-- Grouping enemies for attack

-- | Arrange the enemies into groups for attack
data GroupingPhase = GroupingPhase
  { groupPhaseGroups :: Map GroupId (Set EnemyId)
  }

startGroupPhase :: Set EnemyId -> GroupingPhase
startGroupPhase es =
  GroupingPhase
    { groupPhaseGroups = Map.fromList (zip [ 0 .. ] singletons)
    }
  where
  singletons = [ Set.singleton e | e <- Set.toList es ]

groupPhaseDone :: GroupingPhase -> Bool
groupPhaseDone = (<= 1) . Map.size . groupPhaseGroups


--------------------------------------------------------------------------------
-- Attack Phase

-- | Ranged or normal attack
data AttackPhase = AttackPhase
  { isRangedPhase :: Bool
  , attackGroups  :: Map GroupId AttackGroup
  }

data AttackGroup = AttackGroup
  { groupEnemies :: Set EnemyId
  -- + damage
  }

newAttackGroup :: Set EnemyId -> AttackGroup
newAttackGroup es =
  AttackGroup
    { groupEnemies = es
    }


startAttackPhase :: Bool -> Map GroupId (Set EnemyId) -> AttackPhase
startAttackPhase rng gs =
  AttackPhase
    { isRangedPhase = rng
    , attackGroups = newAttackGroup <$> gs
    }



