module Combat where

import GHC.Generics
import Data.List(partition)
import Data.Maybe(fromMaybe,isNothing)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Aeson(ToJSON(..))
import Optics

import KOI.Bag

import Common
import Terrain.Type(Addr)
import Terrain.Map(Land)
import Enemies

-- | The overall state of the battle
data Combat = Combat
  { _combatEnemies        :: Map EnemyId ActiveEnemy
  , _combatFortifiedSites :: Set SiteId
  , _combatCompleted      :: [CombatPhase]
  , _combatPhase          :: CombatPhase
  , _combatTodoBlock      :: [OneEnemyAttack]
  , _combatTodoWounds     :: Bag Damage
  , _combatLand           :: Land -- ^ for summoning, and update as killed
  }

data CombatPhase =
    Attacking OneAttack
  | Blocking OneBlock
  | AssigningDamage (Maybe Damage) -- ^ Damage currently being assigned, if any

type SiteId     = Addr
type GroupId    = Int
type EnemyId    = Int
type AttackId   = Int

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
  }

-- | A single attack of an enemy.
data OneEnemyAttack = OneEnemyAttack
  { attacker      :: EnemyId      -- ^ Which enemy
  , attackNumber  :: AttackId     -- ^ Which attack (for multi attacks)
  }

data OneBlock = OneBlock
  { _blockingEnemy :: Maybe OneEnemyAttack  -- ^ Who are we blocking
  , _blockAmount   :: Bag Element           -- ^ Amount of block
  }




makeLenses ''Combat
makeLenses ''ActiveEnemy
makeLenses ''OneAttack
makeLenses ''OneBlock
makePrisms ''CombatPhase

-- | Get the fortifications for an enemy.
enemyCurrentFortifications :: Combat -> ActiveEnemy -> Int
enemyCurrentFortifications combat en
  | Unfortified `Set.member` abilities = 0
  | otherwise                          = fromSite + fromAbility
  where
  abilities = enemyAbilities (enemyToken en)

  fromSite
    | enemySite en `Set.member` view combatFortifiedSites combat = 1
    | otherwise                                                      = 0

  fromAbility
    | Fortified `Set.member` abilities = 1
    | otherwise                        = 0

enemyCurrentArmor :: ActiveEnemy -> Int
enemyCurrentArmor ae
  | view enemyIsFullyBlocked ae = armorIfBlocked armor
  | otherwise                       = armorDefault armor
  where
  armor = view enemyEffectiveArmor ae

liveEnemies :: Combat -> Set EnemyId
liveEnemies =
  Map.keysSet . Map.filter (view enemyAlive) . view combatEnemies

blockableEnemies :: Combat -> Set EnemyId
blockableEnemies = Map.keysSet . Map.filter mayBlock . view combatEnemies
  where
  mayBlock e =
    view enemyAlive e && view enemyAttacks e && not (view enemyIsFullyBlocked e)

killEnemy :: EnemyId -> Combat -> Combat
killEnemy eid =
    over (combatEnemies % at eid) \mb ->
    case mb of
      Nothing -> Nothing
      Just e  -> Just (set enemyAlive False (set enemyAttacks False e))

data EndBattle = EndBattle
  { defatedEnemeis    :: [Enemy]              -- ^ Not including summoned
  , undefeatedEnemies :: Map SiteId [Enemy]
  }


--------------------------------------------------------------------------------

data BattleSite = BattleSite
  { siteId        :: SiteId
  , siteFortified :: Bool
  , siteEnemies   :: [Enemy]
  }

-- | Start a battle
startCombat :: Land -> [BattleSite] -> Combat
startCombat land sites =
  Combat
    { _combatEnemies        = allCombatEnemies
    , _combatFortifiedSites = allFortifiedSites
    , _combatCompleted      = []
    , _combatTodoBlock      = []
    , _combatTodoWounds     = bagEmpty
    , _combatPhase          = Attacking (startAttackPhase True)
    , _combatLand           = land
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
    }

attackEnemies :: Combat -> OneAttack -> [ActiveEnemy]
attackEnemies ba att =
  [ e
  | eid      <- Set.toList (view attackGroup att)
  , Just e   <- [Map.lookup eid (view combatEnemies ba)]
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
attackWon ba ap = attackValue resists (view attackDamage ap) >= armor
  where
  (armor,resists) = attackEnemyArmorAndResitance ba ap

--------------------------------------------------------------------------------

startBlockPhase :: OneBlock
startBlockPhase =
  OneBlock
    { _blockingEnemy = Nothing
    , _blockAmount = bagEmpty
    }

-- | How much we need to block.   Assumes no summoner.
blockNeeded :: Combat -> OneBlock -> (Int, Element)
blockNeeded combat bp =
  fromMaybe (0,Physical)
  do att  <- view blockingEnemy bp
     ae   <- Map.lookup (attacker att) (view combatEnemies combat)
     (el,amt) <-
       case splitAt (attackNumber att) (view enemyEffectiveAttack ae) of
         (_,AttacksWith el amt : _) -> pure (el,amt)
         _ -> Nothing
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


-- | Is this a good place to end a phase
mayEndPhase :: Combat -> Bool
mayEndPhase combat =
  case view combatPhase combat of
    Attacking a         -> Set.null (view attackGroup a)
    Blocking bl         -> isNothing (view blockingEnemy bl)
    AssigningDamage dmg ->
      isNothing dmg && bagIsEmpty (view combatTodoWounds combat)

-- | Is it the case that there is nothing else to do in this phase.
phaseFinished :: Combat -> Bool
phaseFinished combat =
  mayEndPhase combat &&
  case view combatPhase combat of
    Attacking {}       -> Set.null (liveEnemies combat)
    Blocking {}        -> null (view combatTodoBlock combat)
    AssigningDamage {} -> True

-- | Try to advance to the next phase where there's something to do.
nextPhase :: Combat -> Either EndBattle Combat
nextPhase = undefined {-combat
  | not (mayEndPhase ph) = Right combat
  | Just ph <- after =
    let new = set combatPhase ph
    in if phaseFinished new then nextPhase new else Right new
  | otherwise = undefined -- End Battle
  where
  ph = view combatPhase combat

  -- XXX: summon
  after =
    case ph of
      Attacking a
        | isRangedAttack -> Just (Blocking startBlockPhase)
        | otherwise -> Nothing

      Blocking {} ->
        Just (AssigningDamage DamagePhase { _unassignedDamage = undefined })

      AssigningDamage {} -> Just (Attacking (startAttackPhase False))
-}

--------------------------------------------------------------------------------

data BattleView = BattleView
  { viewEnemiesUnselected :: [EnemyView]
  , viewEnemiesSelected   :: [EnemyView]
  , viewDamage            :: [(Damage,Int)]
  , viewBattlePhase       :: BattlePhaseView
  } deriving (Generic,ToJSON)

data EnemyView = EnemyView
  { evId      :: EnemyId
  , evEnemy   :: ActiveEnemy
  , evAttack  :: Maybe AttackId
  } deriving (Generic,ToJSON)

data BattlePhaseView =
    RangedAttackView Int Int      -- ^ how much we have, how much we need
  | BlockView Int Int             -- ^ how much we have, how much we need
  | AttackView Int Int            -- ^ how much we have, how much we need
  | DamageView (Maybe Damage)     -- ^ The selected damage, if any
    deriving (Generic,ToJSON)



instance ToJSON Combat where
  toJSON = toJSON . combatView

combatView :: Combat -> BattleView
combatView combat =
  case view combatPhase combat of
    AssigningDamage dmg ->
      BattleView
        { viewEnemiesUnselected = live
        , viewEnemiesSelected = []
        , viewDamage = todoDmg
        , viewBattlePhase = DamageView dmg
        }

    Attacking ap ->
      BattleView
        { viewEnemiesUnselected = unsel
        , viewEnemiesSelected = sel
        , viewDamage = todoDmg
        , viewBattlePhase =
          if isRangedAttack ap
            then RangedAttackView us them
            else AttackView us them
        }
      where
      (them, resist) = attackEnemyArmorAndResitance combat ap
      us = attackValue resist (view attackDamage ap)
      (sel,unsel) = partition match live
      match x = evId x `Set.member` view attackGroup ap

    Blocking bp ->
      BattleView
        { viewEnemiesUnselected = unsel
        , viewEnemiesSelected = sel
        , viewDamage = todoDmg
        , viewBattlePhase = BlockView have need
        }
      where
      sel =
        case view blockingEnemy bp of
          Nothing  -> []
          Just att ->
            [ e { evAttack = Just (attackNumber att) }
            | e <- live, evId e == attacker att
            ]

      unsel =
        [ EnemyView { evId = eid
                    , evEnemy = e
                    , evAttack = Just (attackNumber oneA)
                    }
        | oneA <- view combatTodoBlock combat
        , let eid = attacker oneA
        , Just e <- [ Map.lookup eid (view combatEnemies combat) ]
        ]

      (need, attackEl)  = blockNeeded combat bp
      have              = blockValue attackEl (view blockAmount bp)

  where
  todoDmg = bagToNumList (view combatTodoWounds combat)
  live =
    [ EnemyView { evId = x, evEnemy = e, evAttack = Nothing }
    | (x,e) <- Map.toList (view combatEnemies combat)
    , view enemyAlive e
    ]


