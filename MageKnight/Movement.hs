{-# LANGUAGE Safe, RecordWildCards #-}
module MageKnight.Movement where

import MageKnight.Common
import MageKnight.Terrain

import           Data.Map ( Map )
import qualified Data.Map as Map

data MovePhase = MovePhase
  { mpMove          :: Int
  , mpInfluence     :: Int
  , mpHeal          :: Int
  , mpTerrainCosts  :: Map Terrain Int
  , mpMode          :: MoveMode
  , mpRadius        :: Int
  }

data MoveMode = Walking
              | UsingWingsOfWind Int
              | UsingUndergroundTravel Int

newMovePhase :: Time -> MovePhase
newMovePhase time = MovePhase
  { mpMove         = 0
  , mpInfluence    = 0
  , mpHeal         = 0
  , mpTerrainCosts = terrainCostsDuring time
  , mpMode         = Walking
  , mpRadius       = 1
  }

addMove :: Int -> MovePhase -> MovePhase
addMove n MovePhase { .. } = MovePhase { mpMove = n + mpMove, .. }

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




