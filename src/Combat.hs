module Combat
  ( CombatPhase(..)
  ) where

import GHC.Generics
import Data.List(partition)
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Aeson(ToJSON(..))

import KOI.Bag
import KOI.Field

import Common
import Terrain.Type(Addr)
import Enemies

type SiteId     = Addr
type GroupId    = Int
type EnemyId    = Int

-- | The overall state of the battle
data BattleContext = BattleContext
  { _battleEnemies        :: Map EnemyId ActiveEnemy
  , _battleFortifiedSites :: Set SiteId
  }

--  | A single participant in a battle
data ActiveEnemy = ActiveEnemy
  { enemyToken            :: Enemy          -- ^ Original enemy stats  
  , enemySite             :: SiteId         -- ^ Where we reside
  , enemyIsSummoned       :: Maybe EnemyId  -- ^ Who did we get summoned by
  , _enemyAlive           :: Bool           -- ^ Has this enemy been killed
  , _enemyAttacks         :: Bool           -- ^ Should this enemy attack
  , _enemyEffectiveAttack :: [EnemyAttack]  -- ^ Current is front
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

-- | Ranged or normal attack
data AttackPhase = AttackPhase
  { isRangedPhase   :: Bool           -- ^ True if we are in the ranged phase
  , _attackContext  :: BattleContext  -- ^ General info about the battle
  , _attackGroup    :: Set EnemyId    -- ^ Who we are attacking (inv: alive)
  , _attackDamage   :: Bag Element    -- ^ Cumulative damage
  }

data BlockPhase = BlockPhase
  { _blockingEnemy :: Maybe EnemyId     -- ^ Who are we blocking: (inv: alive)
  , _blockContext  :: BattleContext     -- ^ General info about the battle
  , _blockAmount   :: Bag Element       -- ^ Amount of block
  }

data DamagePhase = DamagePhase
  { _unassignedDamage :: [Damage]
  , _damageContext    :: BattleContext
  }

data Damage = Damage
  { isPoison, isParalyzing, isAssasination :: Bool
  } deriving (Eq,Ord, Generic,ToJSON)



declareFields ''BattleContext
declareFields ''ActiveEnemy
declareFields ''AttackPhase
declareFields ''BlockPhase
declareFields ''DamagePhase

-- | Get the fortifications for an enemy.
enemyCurrentFortifications :: BattleContext -> ActiveEnemy -> Int
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

liveEnemies :: BattleContext -> Set EnemyId
liveEnemies =
  Map.keysSet . Map.filter (getField enemyAlive) . getField battleEnemies

data EndBattle = EndBattle
  { defatedEnemeis :: [Enemy]
  , undefeatedEnemies :: Map SiteId [Enemy]
  }

combatContext :: CombatPhase -> BattleContext
combatContext ph =
  case ph of
    Attacking ap -> getField attackContext ap
    Blocking  bp -> getField blockContext bp
    AssigningDamage d -> getField damageContext d


--------------------------------------------------------------------------------

data BattleSite = BattleSite
  { siteId        :: SiteId
  , siteFortified :: Bool
  , siteEnemies   :: [Enemy]
  }

-- | Start a battle
startBattle :: [BattleSite] -> CombatPhase
startBattle sites = Attacking (startAttackPhase True ctxt)
  where
  ctxt =
    BattleContext
      { _battleEnemies        = allBattleEnemies
      , _battleFortifiedSites = allFortifiedSites
      }

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
    Map.insert (Map.size enemies) (activateEnemy siteId Nothing enemy) enemies

activateEnemy :: SiteId -> Maybe EnemyId -> Enemy -> ActiveEnemy
activateEnemy siteId summoner enemy =
  ActiveEnemy
    { enemyToken = enemy
    , enemySite = siteId
    , _enemyAlive = True
    , enemyIsSummoned = summoner
    , _enemyAttacks = True
    , _enemyIsFullyBlocked = False
    , _enemyEffectiveAttack = enemyAttack enemy
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




--------------------------------------------------------------------------------

startAttackPhase :: Bool -> BattleContext -> AttackPhase
startAttackPhase rng ctxt =
  AttackPhase
    { isRangedPhase   = rng
    , _attackGroup    = Set.empty
    , _attackContext  = ctxt
    , _attackDamage   = bagEmpty
    }

attackEnemies :: AttackPhase -> [ActiveEnemy]
attackEnemies ap =
  [ e
  | eid    <- Set.toList (getField attackGroup ap)
  , Just e <- [Map.lookup eid (getField battleEnemies ba)]
  ]
  where ba = getField attackContext ap

attackEnemyArmorAndResitance :: AttackPhase -> (Int, Set Element)
attackEnemyArmorAndResitance ap =
  ( sum (map enemyCurrentArmor es)
  , Set.unions (map (enemyElementalResistances . enemyToken) es)
  )
  where es = attackEnemies ap

attackValue :: Set Element -> Bag Element -> Int
attackValue resists damage =
  sum [ if el `Set.member` resists then div n 2 else n
      |(el,n) <- bagToNumList damage
      ]

attackWon :: AttackPhase -> Bool
attackWon ap = attackValue resists (getField attackDamage ap) >= armor
  where
  (armor,resists) = attackEnemyArmorAndResitance ap

--------------------------------------------------------------------------------

-- | How much we need to block.   Assumes no summoner.
blockNeeded :: BlockPhase -> (Int, Element)
blockNeeded bp =
  fromMaybe (0,Physical)
  do eid <- getField blockingEnemy bp
     ae  <- Map.lookup eid (getField battleEnemies battle)
     AttacksWith el amt : _ <- pure (getField enemyEffectiveAttack ae)
     pure
       if Swift `Set.member` enemyAbilities (enemyToken ae)
         then (2 * amt, el)
         else (amt, el)
  where
  battle = getField blockContext bp

blockValue :: Element -> Bag Element -> Int
blockValue attackElement blockHave =
  sum [ if effective blockEl then blockAmt else div blockAmt 2
      | (blockEl,blockAmt) <- bagToNumList blockHave
      ]
  where
  effective el =
    case attackElement of
      Fire      -> el == Ice || el == ColdFire
      Ice       -> el == Fire || el == ColdFire
      ColdFire  -> el == ColdFire
      Physical  -> True



--------------------------------------------------------------------------------

instance ToJSON CombatPhase where
  toJSON = toJSON . battleView

battleView :: CombatPhase -> BattleView
battleView phase =
  case phase of
    AssigningDamage dmg ->
      BattleView
        { viewEnemiesUnselected = live
        , viewEnemiesSelected = []
        , viewBattlePhase = DamageView (getField unassignedDamage dmg)
        }

    Attacking ap ->
      BattleView
        { viewEnemiesUnselected = unsel
        , viewEnemiesSelected = sel
        , viewBattlePhase =
          if isRangedPhase ap
            then RangedAttackView us them
            else AttackView us them
        }
      where
      (them, resist) = attackEnemyArmorAndResitance ap
      us = attackValue resist (getField attackDamage ap)
      (sel,unsel) = partition match live
      match (eid,_) = eid `Set.member` getField attackGroup ap

    Blocking bp ->
      BattleView
        { viewEnemiesUnselected = unsel
        , viewEnemiesSelected = sel
        , viewBattlePhase = BlockView have need
        }
      where
      (sel,unsel)       = partition match live
      match (eid,_)     = Just eid == getField blockingEnemy bp
      (need, attackEl)  = blockNeeded bp
      have              = blockValue attackEl (getField blockAmount bp)


  where
  ba = combatContext phase
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
    RangedAttackView Int Int      -- ^ how much we have, how much we need
  | BlockView Int Int             -- ^ how much we have, how much we need
  | AttackView Int Int            -- ^ how much we have, how much we need
  | DamageView [Damage]
    deriving (Generic,ToJSON)



