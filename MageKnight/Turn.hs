{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
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
  , bendSpace, oneMoveLimit

  ) where

import MageKnight.Common ( Element(..), AttackType(..), Mana(..)
                         , BasicMana(..), Time
                         )
import MageKnight.Terrain (Terrain(..), terrainCostsDuring)
import MageKnight.Bag
import MageKnight.JSON
import MageKnight.Movement

import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Control.Monad ( guard )



-- | Resources used to perform actions.
data Resource = Move
                -- Move or explore

              | Heal
                -- Heal

              | ReadyUnit Int
                -- ^ Ready unit of up to this level

              | Attack AttackType Element
                -- Make attacks

              | Block Element
                -- ^ Block attacks

              | Influence
                -- ^ For interacting

              | ManaToken Mana
                -- ^ Power-up stuff

              | ManaDie
                -- ^ Take a die from the source
                deriving (Eq,Ord,Show)

data Phase    = TurnStart Time
              | TurnMovement MovePhase
              | TurnInteract -- XXX
              | TurnCombat -- XXX
              | TurnStandardRest -- XXX
              | TurnSlowRecovery  -- XXX

-- | The state of a player's turn.
data Turn = Turn
  { turnResources :: Bag Resource
  , spentCrystals :: Bag BasicMana
  , turnPhase     :: Phase
  }

-- | Make a new turn, for the given time.
newTurn :: Time -> Turn
newTurn t = Turn
  { turnResources = bagAdd 1 ManaDie bagEmpty
  , spentCrystals = bagEmpty
  , turnPhase     = TurnStart t
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
  case turnPhase t of
    TurnStart s -> payToExplore t { turnPhase = TurnMovement (newMovePhase s) }
    TurnMovement MovePhase { mpMode = Walking } -> spendResource 2 Move t
    _                                           -> Nothing

-- | Pay the price for moving onto the given tile.
-- Assumes that the tile is actually accessible.
payToMove :: Terrain -> Turn -> Maybe Turn
payToMove terrain t0 =
  case turnPhase t0 of
    TurnStart s -> payToMove terrain
                              t0 { turnPhase = TurnMovement (newMovePhase s) }
    TurnMovement mp ->
      do (c,mp1) <- tryToMove terrain mp
         t1      <- spendResource c Move t0
         return t1 { turnPhase = TurnMovement mp1 }
    _ -> Nothing

--------------------------------------------------------------------------------


instance ExportAsKey Resource where
  toKeyJS r =
    case r of
      Move        -> "move"
      Heal        -> "heal"
      Influence   -> "influence"
      Attack t e  -> Text.concat [ "attack_", toKeyJS t, "_", toKeyJS e ]
      Block e     -> Text.append "block_" (toKeyJS e)
      ManaToken m -> toKeyJS m
      ManaDie     -> "mana_die"

instance Export Turn where
  toJS Turn { .. } = object
    [ "resources"       .= object [ toKeyJS r .= x
                                    | (r,x) <- bagToListGrouped turnResources ]
    , "spent_crystals"  .= bagToList spentCrystals


    ]
      {-
    , "terrainCosts"    .= object [ toKeyJS t .= c
                                    | (t,c) <- Map.toList terrainCosts ]
    , "move_mode"       .= (case moveMode of
                              Walking                -> "Normal"
                              UsingWingsOfWind n     ->
                                Text.pack ("Song of wind " ++ show n)
                              UsingUndergroundTravel n ->
                                Text.pack ("Underground " ++ show n)
                           )

    , "move_limit"      .= moveRadius

    ] -}

