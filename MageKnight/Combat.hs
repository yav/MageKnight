module MageKnight.Combat where

import MageKnight.Enemies

import           Data.Set (Set)
import qualified Data.Set as Set

data ActiveEnemy = ActiveEnemy
  { enemyId       :: Int    -- ^ Used to distinguish enemies during a combat
  , enemyFortLoc  :: Bool   -- ^ In a fortified location
  , enemyStats    :: Enemy  -- ^ Current enemy stats
  }

instance Eq ActiveEnemy where
  x == y = enemyId x == enemyId y

instance Ord ActiveEnemy where
  compare x y = compare (enemyId x) (enemyId y)

data CombatPhase = RangedAttackPhase (Set ActiveEnemy)
                    -- ^ Enemies that we are targeting at the moment

                 | BlockingPhase (Maybe ActiveEnemy) -- Currently blocking
                                 (Set ActiveEnemy)   -- Blocked

                 | AttackPhase (Set ActiveEnemy)
                   -- ^ Enemies that we are attacking at the moment

data Enemies = Enemies
  { activeEnemies   :: Set ActiveEnemy
  , defeatedEnemies :: Set ActiveEnemy
  , combatPhase     :: CombatPhase
  }

startCombat :: [(Enemy,Bool)] -> Enemies
startCombat foes = Enemies
  { activeEnemies   = Set.fromList (zipWith activate [0 .. ] foes)
  , defeatedEnemies = Set.empty
  , combatPhase     = RangedAttackPhase Set.empty
  }
  where
  activate n (e,inFort) = ActiveEnemy { enemyId      = n
                                      , enemyFortLoc = inFort
                                      , enemyStats   = e
                                      }






