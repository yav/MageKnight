{-# LANGUAGE Safe, RecordWildCards #-}
module MageKnight.Combat where

import MageKnight.Common
import MageKnight.Enemies
import MageKnight.Bag

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Either (partitionEithers)
import           Data.Foldable (any, foldr)
import           Prelude hiding (any, foldr)

data CombatReason =
    AttackFortified
  | AttackAdventure
  | BurnMonastery
  | AttackRampaging
  | ProvokeRampaging

-- As it happens, these are exactly the "forced" combat.
mayAddRampagingNeighbours :: CombatReason -> Bool
mayAddRampagingNeighbours reason =
  case reason of
    AttackFortified  -> True
    ProvokeRampaging -> True
    _                -> False


data ActiveEnemy = ActiveEnemy
  { enemyId       :: Int    -- ^ Used to distinguish enemies during a combat
  , enemyFortLoc  :: Bool   -- ^ In a fortified location
  , enemyStats    :: Enemy  -- ^ Current enemy stats
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

data CombatPhase = RangedAttackPhase (Set ActiveEnemy)
                    -- ^ Enemies that we are targeting at the moment

                 | BlockingPhase (Maybe ActiveEnemy) -- Currently blocking
                                 (Set ActiveEnemy)   -- Blocked

                 | AssignDamage Int
                   -- ^ This is how much damage needs to get assigned.

                 | AttackPhase (Set ActiveEnemy)
                   -- ^ Enemies that we are attacking at the moment

-- XXX: Some cards make enemies not attack;  we need to rememebr that
data Battle = Battle
  { activeEnemies   :: Map Int ActiveEnemy
  , defeatedEnemies :: Set ActiveEnemy
  , combatPhase     :: CombatPhase
  }

newBattle :: [(Enemy,Bool)] -> Battle
newBattle foes = Battle
  { activeEnemies   = Map.fromList (zipWith activate [0 .. ] foes)
  , defeatedEnemies = Set.empty
  , combatPhase     = RangedAttackPhase Set.empty
  }
  where
  activate n (e,inFort) = (n, ActiveEnemy { enemyId      = n
                                          , enemyFortLoc = inFort
                                          , enemyStats   = e
                                          })



-- | Pick an enemy to attack or block, depending on the current
-- phase of the battle.
selectTarget :: Int -> Battle -> Maybe Battle
selectTarget n Battle { .. } =
  case ( combatPhase
       , Map.updateLookupWithKey (\_ _ -> Nothing) n activeEnemies
       ) of

    (RangedAttackPhase es, (Just e, m1)) ->
      Just Battle { activeEnemies = m1
                  , combatPhase = RangedAttackPhase (Set.insert e es)
                  , ..
                  }

    (BlockingPhase Nothing bs, (Just e, m1)) ->
      Just Battle { activeEnemies = m1
                  , combatPhase = BlockingPhase (Just e) bs
                  , ..
                  }

    (AttackPhase es, (Just e, m1)) ->
      Just Battle { activeEnemies = m1
                  , combatPhase = AttackPhase (Set.insert e es)
                  , ..
                  }
    _ -> Nothing


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
successfulAttack attackTypes enemies =
  totalAttack >= foldr addArmor 0 enemies
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
      isEfficient = case ty of
                      Physycal -> \_ -> True
                      Fire     -> \x -> x == Ice  || x == ColdFire
                      Ice      -> \x -> x == Fire || x == ColdFire
                      ColdFire -> (== ColdFire)

    Summoner -> False -- XXX: Should not happen.




