{-# LANGUAGE RecordWildCards #-}
module MageKnight.Turn
  ( -- * New turn
    Turn, newTurn

    -- * Resources
  , Resource(..)

    -- ** Gaining
  , useCrystal
  , addResource

    -- ** Spending
  , useManaDie
  , payToExplore
  , payToMove

  -- * Adjusting terrain costs
  , setTerrainCost
  , decreaseTerrainCost

  -- * Transporation modes
  , useWalking
  , useWingsOfWind
  , useUndergroundTravel
  , endOfMoveMode

  -- * Bending space
  , bendSpace, isBentSpace

  ) where

import MageKnight.Common ( Element(..), AttackType(..), Mana(..)
                         , BasicMana(..), Time
                         )
import MageKnight.Terrain (Terrain(..), terrainCostsDuring)
import MageKnight.Bag

import           Data.Maybe ( fromMaybe )
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Control.Monad ( guard )



-- | Resources used to perform actions.
data Resource = Move
                -- Move or explore

              | Heal
                -- Heal

              | Attack AttackType
                -- Make attacks

              | Block Element
                -- ^ Block attacks

              | ManaToken Mana
                -- ^ Power-up stuff

              | ManaDie
                -- ^ Take a die from the source
                deriving (Eq,Ord,Show)

-- | The state of a player's turn.
data Turn = Turn
  { turnResources :: Bag Resource
  , terrainCosts  :: Map Terrain Int
  , moveMode      :: MoveMode
  , bentSpace     :: Bool
  , spentCrystals :: Bag BasicMana
  }

-- | Make a new turn, for the given time.
newTurn :: Time -> Turn
newTurn t = Turn
  { turnResources = bagAdd 1 ManaDie bagEmpty
  , terrainCosts  = terrainCostsDuring t
  , moveMode      = Walking
  , bentSpace     = False
  , spentCrystals = bagEmpty
  }

-- | Add some amount of resources.
addResource :: Int -> Resource -> Turn -> Turn
addResource a r Turn { .. } =
  Turn { turnResources = bagAdd a r turnResources, .. }

spendResource :: Int -> Resource -> Turn -> Maybe Turn
spendResource a r Turn { .. } =
  do rs <- bagRemove a r turnResources
     return Turn { turnResources = rs, .. }

-- | Convert a crystal to a mana token.
useCrystal :: BasicMana -> Turn -> Turn
useCrystal t Turn { .. } =
  addResource 1 (ManaToken (BasicMana t))
  Turn { spentCrystals = bagAdd 1 t spentCrystals, .. }

-- | Use a mana die from the source.
useManaDie :: Mana -> Turn -> Maybe Turn
useManaDie m t =
  do t1 <- spendResource 1 ManaDie t
     return (addResource 1 (ManaToken m) t1)

-- | Perform an exploration action.
payToExplore :: Turn -> Maybe Turn
payToExplore t =
  case moveMode t of
    Walking -> spendResource 2 Move t
    _       -> Nothing

-- | Pay the price for moving onto the given tile.
-- Assumes that the tile is actually accessible.
payToMove :: Terrain -> Turn -> Maybe Turn
payToMove terrain t0 =
  case moveMode t0 of
    Walking ->
      do cost <- Map.lookup terrain (terrainCosts t0)
         spendResource cost Move t0

    UsingUndergroundTravel n ->
      do guard (n > 0 && terrain /= Lake && terrain /= Swamp)
         return t0 { moveMode = UsingUndergroundTravel (n-1) }

    UsingWingsOfWind n ->
      do guard (n > 0)
         t1 <- spendResource 1 Move t0
         return t1 { moveMode = UsingWingsOfWind (n-1) }

--------------------------------------------------------------------------------

data MoveMode = Walking
              | UsingWingsOfWind Int
              | UsingUndergroundTravel Int

-- | Switch to walking.
useWalking :: Turn -> Turn
useWalking Turn { .. } = Turn { moveMode = Walking, .. }

-- | Start a special movement mode.  Used for "Wings of Wind".
useWingsOfWind :: Turn -> Maybe Turn
useWingsOfWind Turn { .. } =
  case moveMode of
    Walking -> Just Turn { moveMode = UsingWingsOfWind 5, .. }
    _       -> Nothing

-- | Start a special moveMode mode.
-- Used for "Underground Travel" and "Underground Attack" modes.
useUndergroundTravel :: Turn -> Maybe Turn
useUndergroundTravel Turn { .. } =
  case moveMode of
    Walking -> Just Turn { moveMode = UsingUndergroundTravel 3, .. }
    _       -> Nothing

-- | Is the current special movement mode at an end.
endOfMoveMode :: Turn -> Bool
endOfMoveMode Turn { .. } =
  case moveMode of
    Walking                  -> False
    UsingWingsOfWind n       -> n == 0
    UsingUndergroundTravel n -> n == 0


--------------------------------------------------------------------------------

-- | Switch to "bent space" mode
bendSpace :: Turn -> Turn
bendSpace Turn { .. } = Turn { bentSpace = True, .. }

-- | Is space bent?
isBentSpace :: Turn -> Bool
isBentSpace Turn { .. } = bentSpace

--------------------------------------------------------------------------------

-- | Set the terrain cost to the given amount, unless it is already cheaper.
setTerrainCost :: Terrain -> Int -> Turn -> Turn
setTerrainCost t c Turn { .. } =
  Turn { terrainCosts = Map.insertWith min t c terrainCosts, .. }

-- | Decrease the cost for the given terrain by the given amount,
-- with an optional lower bound.
decreaseTerrainCost :: Terrain -> Int -> Maybe Int -> Turn -> Turn
decreaseTerrainCost t c mbLim Turn { .. } =
  Turn { terrainCosts = Map.adjust decrease t terrainCosts, .. }
  where decrease cur = max (cur - c) lim
        lim          = fromMaybe 0 mbLim





