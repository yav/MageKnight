module Combat
  ( Combat(..)
  , combatPhase
  , CombatPhase(..)
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
import Deed.Type

-- | The overall state of the battle
data Combat = Combat
  { _combatEnemies        :: Map EnemyId ActiveEnemy
  , _combatFortifiedSites :: Set SiteId
  , _combatCompleted      :: [CombatPhase]
  , _combatPhase          :: CombatPhase
  }

data CombatPhase =
    Attacking OneAttack
  | Blocking OneBlock
  | AssigningDamage DamagePhase

type SiteId     = Addr
type GroupId    = Int
type EnemyId    = Int

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

-- | Ranged or normal attack
data OneAttack = OneAttack
  { isRangedAttack  :: Bool           -- ^ True if we are in the ranged phase
  , _attackGroup    :: Set EnemyId    -- ^ Who we are attacking
  , _attackDamage   :: Bag Element    -- ^ Cumulative damage
  , _attakDeeds     :: [Deed]
  }

data OneBlock = OneBlock
  { _blockingEnemy :: Maybe EnemyId     -- ^ Who are we blocking
  , _blockAmount   :: Bag Element       -- ^ Amount of block
  , _blockDeeds    :: [Deed]
  }

data DamagePhase = DamagePhase
  { _unassignedDamage :: [Damage]
  }

data Damage = Damage
  { isPoison, isParalyzing, isAssasination :: Bool
  } deriving (Eq,Ord, Generic,ToJSON)



declareFields ''Combat
declareFields ''ActiveEnemy
declareFields ''OneAttack
declareFields ''OneBlock
declareFields ''DamagePhase

-- | Get the fortifications for an enemy.
enemyCurrentFortifications :: Combat -> ActiveEnemy -> Int
enemyCurrentFortifications combat en
  | Unfortified `Set.member` abilities = 0
  | otherwise                          = fromSite + fromAbility
  where
  abilities = enemyAbilities (enemyToken en)

  fromSite
    | enemySite en `Set.member` getField combatFortifiedSites combat = 1
    | otherwise                                                      = 0

  fromAbility
    | Fortified `Set.member` abilities = 1
    | otherwise                        = 0

enemyCurrentArmor :: ActiveEnemy -> Int
enemyCurrentArmor ae
  | getField enemyIsFullyBlocked ae = armorIfBlocked armor
  | otherwise                       = armorDefault armor
  where
  armor = getField enemyEffectiveArmor ae

liveEnemies :: Combat -> Set EnemyId
liveEnemies =
  Map.keysSet . Map.filter (getField enemyAlive) . getField combatEnemies

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
startCombat :: [BattleSite] -> Combat
startCombat sites =
  Combat
    { _combatEnemies        = allCombatEnemies
    , _combatFortifiedSites = allFortifiedSites
    , _combatCompleted      = []
    , _combatPhase          = Attacking (startAttackPhase True)
    }

  where
  (allCombatEnemies,allFortifiedSites) =
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

startAttackPhase :: Bool -> OneAttack
startAttackPhase rng =
  OneAttack
    { isRangedAttack  = rng
    , _attackGroup    = Set.empty
    , _attackDamage   = bagEmpty
    , _attakDeeds     = []
    }

attackEnemies :: Combat -> OneAttack -> [ActiveEnemy]
attackEnemies ba at =
  [ e
  | eid      <- Set.toList (getField attackGroup at)
  , Just e   <- [Map.lookup eid (getField combatEnemies ba)]
  ]

attackEnemyArmorAndResitance :: Combat -> OneAttack -> (Int, Set Element)
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

attackWon :: Combat -> OneAttack -> Bool
attackWon ba ap = attackValue resists (getField attackDamage ap) >= armor
  where
  (armor,resists) = attackEnemyArmorAndResitance ba ap

--------------------------------------------------------------------------------

-- | How much we need to block.   Assumes no summoner.
blockNeeded :: Combat -> OneBlock -> (Int, Element)
blockNeeded combat bp =
  fromMaybe (0,Physical)
  do eid <- getField blockingEnemy bp
     ae  <- Map.lookup eid (getField combatEnemies combat)
     AttacksWith el amt : _ <- pure (getField enemyEffectiveAttack ae)
     pure
       if Swift `Set.member` enemyAbilities (enemyToken ae)
         then (2 * amt, el)
         else (amt, el)

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



instance ToJSON Combat where
  toJSON = toJSON . combatView

combatView :: Combat -> BattleView
combatView combat =
  case getField combatPhase combat of
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
          if isRangedAttack ap
            then RangedAttackView us them
            else AttackView us them
        }
      where
      (them, resist) = attackEnemyArmorAndResitance combat ap
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
      (need, attackEl)  = blockNeeded combat bp
      have              = blockValue attackEl (getField blockAmount bp)


  where
  live =
    [ (x, e)
    | (x,e) <- Map.toList (getField combatEnemies combat)
    , getField enemyAlive e
    ]


