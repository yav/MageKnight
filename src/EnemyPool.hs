module EnemyPool
  ( EnemyPool
  , newEnemyPool
  , getEnemy
  , returnEnemy
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(foldM)
import KOI.RNGM
import KOI.ResourceQ
import Enemies

newtype EnemyPool = EnemyPool (Map EnemyType (ResourceQ Enemy))

-- | Use exactly these enemies.
newEnemyPool :: [Enemy] -> Gen EnemyPool
newEnemyPool enemies = do pool <- blank
                          pure (EnemyPool (foldr add pool enemies))
  where
  add e qs = Map.adjust (rqDiscard e) (enemyType e) qs
  blank    = foldM addQ Map.empty allEnemyTypes
  addQ m e = do q <- rqEmpty
                pure (Map.insert e q m)

-- | Get an enemy of the given type, if any.
getEnemy :: EnemyType -> EnemyPool -> Maybe (Enemy, EnemyPool)
getEnemy et (EnemyPool pool) =
  do q <- Map.lookup et pool
     (e,q1) <- rqTake q
     return (e, EnemyPool (Map.insert et q1 pool))

-- | Return an enemy so that it can be reused.
returnEnemy :: Enemy -> EnemyPool -> EnemyPool
returnEnemy e (EnemyPool pool) =
  EnemyPool (Map.adjust (rqDiscard e) (enemyType e) pool)


