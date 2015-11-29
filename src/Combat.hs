{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
module Combat where

import Common
import Enemies
import Util.Perhaps
import Util.Bag
import Deed

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Foldable (any, foldr)
import           Data.Either(partitionEithers)
import           Prelude hiding (any, foldr)
import           qualified Data.Text as Text


--------------------------------------------------------------------------------

-- | Why are we fighting?
data CombatReason =
    AttackFortified  -- ^ We are attacking a fortified site (e.g., keep)
  | AttackAdventure  -- ^ We are attacking an adventure site (e.g. dungeon)
  | BurnMonastery    -- ^ Oh no!
  | AttackRampaging  -- ^ We are attacking a rampaging enemy (e.g. orcs)
  | ProvokeRampaging -- ^ We are being attacked by provoked rampaging enemy.

-- | For some fights, we are also allowed to challenge rampaging enemies
-- adjecent to the combat site.
-- As it happens, these are exactly the "forced" combat.
mayAddRampagingNeighbours :: CombatReason -> Bool
mayAddRampagingNeighbours reason =
  case reason of
    AttackFortified  -> True
    ProvokeRampaging -> True
    _                -> False




-- | An enemy in action.  Used while we are fighting it.
data ActiveEnemy = ActiveEnemy
  { enemyId         :: Int    -- ^ Used to distinguish enemies during a combat
  , enemyFortLoc    :: Bool   -- ^ In a fortified location
  , enemyWillAttack :: Bool   -- ^ Usually yes, but it might be disabled.

  , enemyOrigStats  :: Enemy
    -- ^ Original stats of the enemy.
    -- Some abilities refer to the stats printed on the actual token.
    -- (e.g. cold toughness gives bonuses only for what's on the token)

  , enemyStats      :: Enemy
    -- ^ Current enemy stats, which may include various bonuses
    -- (e.g., due to city bonuses), or be updated by casting spells
    -- on the enemy.
  }

enemyHasAbility :: EnemyAbility -> ActiveEnemy -> Bool
enemyHasAbility x = Set.member x . enemyAbilities . enemyStats

enemyResists :: Element -> ActiveEnemy -> Bool
enemyResists ColdFire e = enemyHasAbility (Resists Ice) e &&
                          enemyHasAbility (Resists Fire) e
enemyResists x e        = enemyHasAbility (Resists x) e


instance Eq ActiveEnemy where
  x == y = enemyId x == enemyId y

instance Ord ActiveEnemy where
  compare x y = compare (enemyId x) (enemyId y)




--------------------------------------------------------------------------------



--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- | Used for ranged attack, blockinga, and melee attacks.
data CombatPhase = CombatPhase
  { currentSkirmish   :: Maybe Skirmish
    -- ^ What we are doinf currently

  , remainingEnemies  :: Map Int ActiveEnemy
    -- ^ Enemies available for attack

  , deadEnemies       :: [Skirmish]
    -- ^ Enemies that were killed

  , blockedEnemies    :: [Skirmish]
    -- ^ Enemies that were blocked

  , combatPhase       :: CombatPhaseType
    -- ^ What phase are we working on
  }

-- | Combat phases that require playing cards
data CombatPhaseType = RangedAttackPhase
                     | BlockPhase
                     | AssignDamage Int -- ^ Amount left to assign
                     | MeleeAttackPhase

-- | An atcakk of some enemies, or a block of a single enmy.
data Skirmish = Skirmish
  { skirmishEnemies   :: Map Int ActiveEnemy
    -- ^ Whos is being attacked or blocked, indexed by id.
    -- Never empty.  Always singleton during blocking phase.

  , playerActions     :: [ActivedDeed]
    -- ^ Deeds to resolve the attack.  Most revcently played at the front.
  }

-- | Start a ranged attack against some enemies.
startCombat :: [ActiveEnemy] -> CombatPhase
startCombat es = CombatPhase { currentSkirmish  = Nothing
                             , remainingEnemies = Map.fromList (map prep es)
                             , deadEnemies      = []
                             , blockedEnemies   = []
                             , combatPhase      = RangedAttackPhase
                             }
  where prep e = (enemyId e, e)

-- | A skirmish with the given enemies.
newSkirmish :: [ActiveEnemy] -> Skirmish
newSkirmish es = Skirmish { skirmishEnemies = Map.fromList (map prep es)
                          , playerActions   = []
                          }
  where prep e = (enemyId e, e)

-- | Finish current skirmish.
winSkirmish :: CombatPhase -> Perhaps CombatPhase
winSkirmish CombatPhase { .. } =
  perhaps "Not skirmishing." $
  do s <- currentSkirmish
     case combatPhase of
       BlockPhase -> return CombatPhase { currentSkirmish = Nothing
                                        , blockedEnemies  = s : blockedEnemies
                                        , .. }
       AssignDamage _ -> error "[Bug] Skirmish while assigning damage."
       _  -> return CombatPhase { currentSkirmish = Nothing
                                , deadEnemies     = s : deadEnemies
                                , .. }

-- | Attack or block a new group of enemies.
startSkirmish :: [Int] -> CombatPhase -> Perhaps CombatPhase
startSkirmish xs CombatPhase { .. }
  | [] <- xs =
    Failed "Cannot start an empty skirmish."
  | _ : _ : _ <- xs, BlockPhase <- combatPhase =
    Failed "Enemies need to be blocked one at a time."
  | Just _ <- currentSkirmish =
    Failed "Already in a skirmish."
  | otherwise =
    do es <- mapM lkp xs
       return CombatPhase
          { currentSkirmish  = Just (newSkirmish es)
          , remainingEnemies = foldr Map.delete remainingEnemies xs
          , .. }
  where lkp x = perhaps ("Unknown enemy: " `Text.append` Text.pack (show x))
              $ Map.lookup x remainingEnemies

-- | Play a card towards the current skirmish.
makeAttack :: ActivedDeed -> CombatPhase -> Perhaps CombatPhase
makeAttack d CombatPhase { .. } =
  do s <- perhaps "Not skirmishing." currentSkirmish
     return CombatPhase
             { currentSkirmish = Just s { playerActions = d : playerActions s }
             , .. }

-- | Proceed to the next phase of combat.
nextPhase :: CombatPhase -> Perhaps CombatPhase
nextPhase CombatPhase { .. }
  | Just _ <- currentSkirmish =
    Failed "There is skirmish currently in progress."

  | otherwise =
    case combatPhase of

      RangedAttackPhase ->
        return CombatPhase { combatPhase = BlockPhase, .. }

      BlockPhase ->
        return CombatPhase { combatPhase = AssignDamage 0 -- XXX
                           , .. }

      AssignDamage n
        | n < 0     -> error "[Bug] Negative damage to assign"
        | n > 0     -> Failed "More damage needs to be assigned"
        | otherwise -> return CombatPhase { combatPhase = MeleeAttackPhase, .. }

      MeleeAttackPhase ->
        Failed "Mellee attack is the last combat phase."


resolveDamage :: Int -> CombatPhase -> Perhaps CombatPhase
resolveDamage n CombatPhase { .. }
  | AssignDamage d <- combatPhase =
    if d >= n then return CombatPhase { combatPhase = AssignDamage (d - n), .. }
              else Failed "Tried to resolve too much damage."
  | otherwise = Failed "Not assigning damage."

--------------------------------------------------------------------------------




-- | Check if the attacks are sufficient to defeat the set of enemies.
successfulRangedAttack :: Bag Element {-^ Ranged -} ->
                          Bag Element {-^ Siege  -} ->
                          Set ActiveEnemy -> Bool
successfulRangedAttack rangedAttack siegeAttack enemies =
  successfulAttack attackTypes enemies
  where
  inFort    = any enemyFortLoc enemies
  fortified = any (enemyHasAbility Fortified) enemies

  attackTypes
    | inFort    = if fortified then bagEmpty else siegeAttack
    | fortified = siegeAttack
    | otherwise = bagUnion rangedAttack siegeAttack

-- | Check if the attacks are sufficient to defeat the set of enemies.
successfulAttack :: Bag Element -> Set ActiveEnemy -> Bool
successfulAttack attackTypes enemies = totalAttack >= foldr addArmor 0 enemies
  where
  addArmor e tot          = enemyArmor (enemyStats e) + tot
  resists x               = any (enemyResists x) enemies

  classify (el,amt)       = if resists el then Left amt else Right amt
  (inefficient,efficient) = partitionEithers $ map classify
                                             $ bagToListGrouped attackTypes
  totalAttack             = sum (div (sum inefficient) 2 : efficient)


-- | Check if the given amounts of block are sufficient to block an enemy.
successfulBlock :: Bag Element -> ActiveEnemy -> Bool
successfulBlock blocks enemy =
  case enemyAttack (enemyStats enemy) of
    AttacksWith ty amt -> sum (div (sum inefficient) 2 : efficient) >= attack
      where
      attack
        | enemyHasAbility Swift enemy = 2 * amt
        | otherwise                   = amt

      (inefficient,efficient) = partitionEithers $ map classify
                                                 $ bagToListGrouped blocks
      classify (bt,a) = if isEfficient bt then Right a else Left a
      isEfficient bl = case ty {- type of attack -} of
                         Physycal -> True
                         Fire     -> bl == Ice  || bl == ColdFire
                         Ice      -> bl == Fire || bl == ColdFire
                         ColdFire -> bl == ColdFire

    Summoner -> error "[Bug] Tried to block a summoner"



