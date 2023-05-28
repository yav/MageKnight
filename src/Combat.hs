module Combat where

import GHC.Generics
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Aeson(ToJSON)

import KOI.Bag
import KOI.Field

import Common
import Deed.Type
import Terrain.Type(Addr)
import Enemies

type SiteId     = Addr
type GroupId    = Int
type EnemyId    = Int

-- | The overall state of the battle
data Battle = Battle
  { _battleEnemies        :: Map EnemyId ActiveEnemy
  , _battleFortifiedSites :: Set SiteId
  , _battlePhase          :: CombatPhase
  }

--  | A single participant in a battle
data ActiveEnemy = ActiveEnemy
  { enemyToken            :: Enemy          -- ^ Original enemy stats  
  , enemySite             :: SiteId         -- ^ Where we reside
  , enemyIsSummoned       :: Maybe EnemyId  -- ^ Who did we get summoned by
  , _enemyAlive           :: Bool           -- ^ Has this enemy been killed
  , _enemyAttacks         :: Bool           -- ^ Should this enemy attack
  , _enemyIsFullyBlocked  :: Bool
  } deriving (Generic,ToJSON)

data CombatPhase =
    Attacking       AttackPhase     -- ^ Ranged and mellee are both here
  | Blocking        BlockPhase
  | AssigningDamage DamagePhase
    deriving (Generic,ToJSON)

-- | Ranged or normal attack
data AttackPhase = AttackPhase
  { isRangedPhase   :: Bool
  , _attackGroup    :: Set EnemyId
  , _attackDamage   :: Bag Element
  } deriving (Generic,ToJSON)

type BlockPhase = Maybe EnemyId     -- ^ Who are blocking currently

type DamagePhase = [Wound]

data Wound = Wound
  { isPoison, isParalyzing, isAssasination :: Bool
  } deriving (Eq,Ord, Generic,ToJSON)



declareFields ''Battle
declareFields ''ActiveEnemy
declareFields ''AttackPhase

-- | Get the fortifications for an enemy.
enemyFortifications :: Battle -> ActiveEnemy -> Int
enemyFortifications btl en
  | Unfortified `Set.member` abilities = 0
  | otherwise                          = fromSite + fromAbility
  where
  abilities = enemyAbilities (enemyToken en)

  fromSite
    | enemySite en `Set.member` getField battleFortifiedSites btl = 1
    | otherwise                                                   = 0

  fromAbility
    | Fortified `Set.member` abilities = 1
    | otherwise                        = 0



data EndBattle = EndBattle
  { defatedEnemeis :: [Enemy]
  , undefeatedEnemies :: Map SiteId [Enemy]
  }




--------------------------------------------------------------------------------

data BattleSite = BattleSite
  { siteId        :: SiteId
  , siteFortified :: Bool
  , siteEnemies   :: [Enemy]
  }

-- | Start a battle
startBattle :: [BattleSite] -> Battle
startBattle sites =
  Battle
    { _battleEnemies        = allBattleEnemies
    , _battleFortifiedSites = allFortifiedSites
    , _battlePhase          = Attacking (startAttackPhase True)
    }
  where
  (allBattleEnemies,allFortifiedSites) =
    foldr addSite (Map.empty,Set.empty) sites

  addSite site (enemies,fortifiedSites) =
    let sid = siteId site
    in
    ( foldr (addEnemy sid) enemies (siteEnemies site)
    , if siteFortified site
        then Set.insert sid fortifiedSites
        else fortifiedSites
    )

  addEnemy siteId enemy enemies =
    Map.insert (Map.size enemies) (activateEnemy siteId enemy) enemies

  activateEnemy siteId enemy =
    ActiveEnemy
      { enemyToken = enemy
      , enemySite = siteId
      , _enemyAlive = True
      , enemyIsSummoned = Nothing
      , _enemyAttacks = True
      , _enemyIsFullyBlocked = False
      }


nextPhase :: Battle -> Either EndBattle Battle
nextPhase bat =
  case getField battlePhase bat of
    Attacking a -> undefined
    Blocking b -> undefined
    AssigningDamage d -> undefined

--------------------------------------------------------------------------------
-- Attack Phase

startAttackPhase :: Bool -> AttackPhase
startAttackPhase rng =
  AttackPhase
    { isRangedPhase = rng
    , _attackGroup  = Set.empty
    , _attackDamage = bagEmpty
    }


