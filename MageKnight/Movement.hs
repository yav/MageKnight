{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
module MageKnight.Movement
  ( MovePhase
  , newMovePhase
  , useWalking
  , useWingsOfWind
  , useUndergroundTravel
  , bendSpace
  , setTerrainCost
  , decreaseTerrainCost
  , isWalking
  , oneMoveLimit
  , tryToMove
  ) where

import MageKnight.Common
import MageKnight.Terrain
import MageKnight.Perhaps

import           Data.Map ( Map )
import qualified Data.Map as Map

data MovePhase = MovePhase
  { mpTerrainCosts  :: Map Terrain Int
  , mpMode          :: MoveMode
  , mpRadius        :: Int
  }

data MoveMode = Walking
              | UsingWingsOfWind Int
              | UsingUnderground Int UndergroundMode
                deriving (Eq,Show)

data UndergroundMode = UndergroundTravel | UndergroundAttack
                       deriving (Eq,Show)

-- | Start movement phase at the given time of day.
newMovePhase :: Time -> MovePhase
newMovePhase time = MovePhase
  { mpTerrainCosts = terrainCostsDuring time
  , mpMode         = Walking
  , mpRadius       = 1
  }

-- | Switch to walking, unless we are doing an undergounrd attack, in which
-- case we have to attack when done.
useWalking :: MovePhase -> Maybe MovePhase
useWalking MovePhase { .. } =
  case mpMode of
    UsingUnderground _ UndergroundAttack -> Nothing
    _ -> Just MovePhase { mpMode = Walking, .. }

-- | Start a special movement mode.  Used for "Wings of Wind".
useWingsOfWind :: MovePhase -> Maybe MovePhase
useWingsOfWind MovePhase { .. } =
  case mpMode of
    UsingUnderground _ UndergroundAttack -> Nothing
    _ -> Just MovePhase { mpMode = UsingWingsOfWind 5, .. }

-- | Start a special moveMode mode.
-- Used for "Underground Travel" and "Underground Attack" modes.
useUndergroundTravel :: UndergroundMode -> MovePhase -> Maybe MovePhase
useUndergroundTravel m MovePhase { .. } =
  case mpMode of
    UsingUnderground _ UndergroundAttack -> Nothing
    _ -> Just MovePhase { mpMode = UsingUnderground 3 m, .. }

-- | Switch to "bent space" mode
bendSpace :: MovePhase -> MovePhase
bendSpace MovePhase { .. } = MovePhase { mpRadius = 2, .. }

-- | How far from the player can they move/explore in a single turn.
-- Used for "Bend Space"
oneMoveLimit :: MovePhase -> Int
oneMoveLimit MovePhase { .. } = mpRadius

isWalking :: MovePhase -> Bool
isWalking MovePhase { .. } = mpMode == Walking


{- | How much it would cost to move to this type of terrain.
Assumes that the tile is actually accessible.
Returns the cost the terrain, and and updated movement phase,
to be used if there are enough resources to move. -}
tryToMove :: Terrain -> MovePhase -> Perhaps (Int, MovePhase)
tryToMove terrain MovePhase { .. } =
  case mpMode of
    Walking ->
      do c <- perhaps "This terrain is inaccessable."
              $ Map.lookup terrain mpTerrainCosts
         return (c, MovePhase { .. })

    UsingUnderground n m
      | n > 0 ->
         do checkThat (terrain /= Lake) "Cannot move under lakes."
            checkThat (terrain /= Swamp) "Cannot move under swamps."
            return (0, MovePhase { mpMode = UsingUnderground (n-1) m, .. })

      -- XXX: Has to end on a safe space
      | otherwise ->
        do checkThat (m /= UndergroundAttack)
             "An underground attack must end in battle."
           tryToMove terrain MovePhase { mpMode = Walking, .. }

    UsingWingsOfWind n
      | n > 0  -> return (1, MovePhase { mpMode = UsingWingsOfWind (n-1), .. })


      -- XXX: Has to end on a safe space
      | otherwise -> tryToMove terrain MovePhase { mpMode = Walking, .. }



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




