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
  --XXX: effective attack value, store here or in block? multi attacks
  , _enemyEffectiveArmor  :: EffectiveArmor -- ^ Current armor values
  , _enemyIsFullyBlocked  :: Bool
  } deriving (Generic,ToJSON)

data EffectiveArmor = EffectiveArmor
  { armorDefault   :: Int     -- ^ If not fully blocked or didn't attack
  , armorIfBlocked :: Int     -- ^ If fully blocked
  } deriving (Generic,ToJSON)

data CombatPhase =
    Attacking       AttackPhase     -- ^ Ranged and mellee are both here
  | Blocking        BlockPhase
  | AssigningDamage DamagePhase
    deriving (Generic,ToJSON)

-- | Ranged or normal attack
data AttackPhase = AttackPhase
  { isRangedPhase   :: Bool           -- ^ True if we are in the ranged phase
  , _attackGroup    :: Set EnemyId    -- ^ Who we are attacking (inv: alive)
  , _attackDamage   :: Bag Element    -- ^ Cumulative damage
  } deriving (Generic,ToJSON)

data BlockPhase = BlockPhase
  { _blockingEnemy :: Maybe EnemyId     -- ^ Who are we blocking: (inv: alive)
  , _blockAmount   :: Bag Element       -- ^ Amount of block
  } deriving (Generic,ToJSON)

type DamagePhase = [Wound]

data Damage = Damage
  { isPoison, isParalyzing, isAssasination :: Bool
  } deriving (Eq,Ord, Generic,ToJSON)



declareFields ''Battle
declareFields ''ActiveEnemy
declareFields ''AttackPhase

-- | Get the fortifications for an enemy.
enemyCurrentFortifications :: Battle -> ActiveEnemy -> Int
enemyCurrentFortifications btl en
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

enemyCurrentArmor :: ActiveEnemy -> Int
enemyCurrentArmor ae
  | getField enemyIsFullyBlocked ae = armorIfBlocked armor
  | otherwise                       = armorDefault armor
  where
  armor = getField enemyEffectiveArmor ae

liveEnemies :: Battle -> Set EnemyId
liveEnemies =
  Map.keysSet . Map.filter (getField enemyAlive) . getField battleEnemies

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
      , _enemyEffectiveArmor =
          EffectiveArmor
            { armorIfBlocked = enemyArmor enemy
            , armorDefault =
                case enemyIsElusive enemy of
                  Just a -> a
                  Nothing -> enemyArmor enemy
            }
      }


--------------------------------------------------------------------------------
-- Attack Phase

startAttackPhase :: Bool -> AttackPhase
startAttackPhase rng =
  AttackPhase
    { isRangedPhase = rng
    , _attackGroup  = Set.empty
    , _attackDamage = bagEmpty
    }

attackEnemies :: Battle -> AttackPhase -> [ActiveEnemy]
attackEnemies ba ap =
  [ e
  | eid    <- Set.toList (getField attackGroup ap)
  , Just e <- [Map.lookup eid (getField battleEnemies ba)]
  ]

attackEnemyArmorAndResitance :: Battle -> AttackPhase -> (Int, Set Element)
attackEnemyArmorAndResitance ba ap =
  ( sum (map enemyCurrentArmor es)
  , Set.unions (map (enemyElementalResistances . enemyToken) es)
  )
  where es = attackEnemies ba ap

attackValue :: Set Element -> Bag Element -> Int
attackValue resists damage =
  sum [ if el `Set.member` resists then div n 2 else n
      |(el,n) <- bagToNumList damage
      ]

attackWon :: Battle -> AttackPhase -> Bool
attackWon ba ap = attackValue resists (getField attackDamage ap) >= armor
  where
  (armor,resists) = attackEnemyArmorAndResitance ba ap

--------------------------------------------------------------------------------

battleView :: Battle -> BattleView
battleView ba =
  case getField batllePhase ba of
    DamagePhase dmg ->
      BattleView
        { viewEnemiesUnselected = live
        , viewEnemiesSelected = []
        , viewBattlePhase = DamageView dmg
        }

    AttackPhase ap ->
      BattleView
        { viewEnemiesUnselected = unsel
        , viewEnemiesSelected = sle
        , viewBattlePhase =
          if isRangedPhase ap
            then RangedAttackView us them
            else AttackPhase us them
        }
      where
      (them, resist) = attackEnemyArmorAndResitance ba ap
      (sel,unsel) = partition match live
      match (eid,_) = eid `Set.member` getField attackGroup ap

    BlockPhase bp ->
      BattleView
        { viewEnemiesUnselected = unsel
        , viewEnemiesSelected = sel
        , viewBattlePhase =
          BlockPhase undefined undefined
        }
      where
      (sel,unsel) = partition match live
      match (eid,_) = Just eid == getField blockingEnemy bp
 

  where
  live =
    [ (x, e)
    | (x,e) <- Map.toList (getField battleEnemies ba)
    , getField enemyAlive e
    ]

 

data BattleView = BattleView
  { viewEnemiesUnselected :: [(EnemyId,ActiveEnemy)]
  , viewEnemiesSelected   :: [(EnemyId,ActiveEnemy)]
  , viewBattlePhase       :: BattlePhaseView
  } deriving (Generic,ToJSON)

data BattlePhaseView =
    RangedAttackView Int Int      -- ^ us, them
  | BlockView Int Int
  | AttackView Int Int
  | DamageView [Damage]
    deriving (Generic,ToJSON)



