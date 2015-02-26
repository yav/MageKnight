{-# LANGUAGE Safe, RecordWildCards #-}
module MageKnight.Movement where

import MageKnight.Common
import MageKnight.Terrain

import           Control.Monad(guard)
import           Data.Map ( Map )
import qualified Data.Map as Map

data MovePhase = MovePhase
  { mpTerrainCosts  :: Map Terrain Int
  , mpMode          :: MoveMode
  , mpRadius        :: Int
  }

data MoveMode = Walking
              | UsingWingsOfWind Int
              | UsingUndergroundTravel Int

-- | Start movement phase at the given time of day.
newMovePhase :: Time -> MovePhase
newMovePhase time = MovePhase
  { mpTerrainCosts = terrainCostsDuring time
  , mpMode         = Walking
  , mpRadius       = 1
  }

-- | Switch to walking.
useWalking :: MovePhase -> MovePhase
useWalking MovePhase { .. } = MovePhase { mpMode = Walking, .. }

-- | Start a special movement mode.  Used for "Wings of Wind".
useWingsOfWind :: MovePhase -> Maybe MovePhase
useWingsOfWind MovePhase { .. } =
  case mpMode of
    Walking -> Just MovePhase { mpMode = UsingWingsOfWind 5, .. }
    _       -> Nothing

-- | Start a special moveMode mode.
-- Used for "Underground Travel" and "Underground Attack" modes.
useUndergroundTravel :: MovePhase -> Maybe MovePhase
useUndergroundTravel MovePhase { .. } =
  case mpMode of
    Walking -> Just MovePhase { mpMode = UsingUndergroundTravel 3, .. }
    _       -> Nothing

-- | Is the current special movement mode at an end.
endOfMoveMode :: MovePhase -> Bool
endOfMoveMode MovePhase { .. } =
  case mpMode of
    Walking                  -> False
    UsingWingsOfWind n       -> n == 0
    UsingUndergroundTravel n -> n == 0


-- | Switch to "bent space" mode
bendSpace :: MovePhase -> MovePhase
bendSpace MovePhase { .. } = MovePhase { mpRadius = 2, .. }

-- | How far from the player can they move/explore in a single turn.
-- Used for "Bend Space"
oneMoveLimit :: MovePhase -> Int
oneMoveLimit MovePhase { .. } = mpRadius


{- | How much it would cost to move to this type of terrain.
Assumes that the tile is actually accessible.
Returns the cost the terrain, and and updated movement phase,
to be use if there are enough resoutrces to move -}
tryToMove :: Terrain -> MovePhase -> Maybe (Int, MovePhase)
tryToMove terrain MovePhase { .. } =
  case mpMode of
    Walking ->
      do c <- Map.lookup terrain mpTerrainCosts
         return (c, MovePhase { .. })

    UsingUndergroundTravel n ->
      do guard (n > 0 && terrain /= Lake && terrain /= Swamp)
         return (0, MovePhase { mpMode = UsingUndergroundTravel (n-1) })

    UsingWingsOfWind n ->
      do guard (n > 0)
         return (1, MovePhase { mpMode = UsingWingsOfWind (n-1) })



--------------------------------------------------------------------------------

-- | Set the terrain cost to the given amount, unless it is already cheaper.
setTerrainCost :: Terrain -> Int -> MovePhase -> MovePhase
setTerrainCost t c MovePhase { .. } =
  MovePhase { mpTerrainCosts = Map.insertWith min t c mpTerrainCosts, .. }

-- | Decrease the cost for the given terrain by the given amount,
-- to the given lower bounds.
decreaseTerrainCost :: Terrain -> Int -> Int -> MovePhase -> MovePhase
decreaseTerrainCost t c lim MovePhase { .. } =
  MovePhase { mpTerrainCosts = Map.adjust decrease t mpTerrainCosts , .. }
  where decrease cur = max (cur - c) lim




