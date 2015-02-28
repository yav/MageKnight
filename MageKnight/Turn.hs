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
                         , BasicMana(..), Time(..)
                         )
import MageKnight.Terrain (Terrain(..))
import MageKnight.Game
import MageKnight.Player
import MageKnight.Land
import MageKnight.Random
import MageKnight.Bag
import MageKnight.JSON
import MageKnight.Movement

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

                deriving (Eq,Ord,Show)

data Phase    = TurnStart
              | TurnMovement MovePhase
              | TurnInteract -- XXX
              | TurnCombat -- XXX
              | TurnStandardRest -- XXX
              | TurnSlowRecovery  -- XXX

-- | The state of a player's turn.
data Turn = Turn
  { turnGame      :: Game
  , turnTime      :: Time     -- ^ May differ from the game, if in dungeon
  , turnResources :: Bag Resource
  , spentCrystals :: Bag BasicMana
  , manaTokens    :: Bag Mana
  , manaDies      :: Int
  , turnPhase     :: Phase
  }

-- | Make a new turn, for the given time.
newTurn :: Game -> Turn
newTurn g = Turn
  { turnGame      = g
  , turnTime      = getTime (theLand g)
  , turnResources = bagEmpty
  , spentCrystals = bagEmpty
  , manaTokens    = bagEmpty
  , manaDies      = 1
  , turnPhase     = TurnStart
  }

-- | Add some amount of resources.
addResource :: Int -> Resource -> Turn -> Turn
addResource a r Turn { .. } =
  Turn { turnResources = bagAdd a r turnResources, .. }

-- | Spend some amount of resources.
spendResource :: Int -> Resource -> Turn -> Maybe Turn
spendResource a r Turn { .. } =
  do rs <- bagRemove a r turnResources
     return Turn { turnResources = rs, .. }

-- | Modify the game.
updateGame :: Functor f => (Game -> f Game) -> Turn -> f Turn
updateGame f Turn { .. } = fmap (\g1 -> Turn { turnGame = g1, .. }) (f turnGame)


--------------------------------------------------------------------------------


-- | Convert a crystal to a mana token.
useCrystal :: BasicMana -> Turn -> Maybe Turn
useCrystal c t =
  do Turn { .. } <- (updateGame . updatePlayer) (removeCrystal c) t
     return Turn { spentCrystals = bagAdd 1 c spentCrystals
                 , manaTokens    = bagAdd 1 (BasicMana c) manaTokens
                 , ..
                 }

-- | Use a mana die from the source.
useManaDie :: Mana -> Turn -> Maybe Turn
useManaDie m t =
  do guard (manaDies t >= 1)
     guard (case turnTime t of
              Day   -> m /= Black
              Night -> m /= Gold)
     Turn { .. } <- updateGame (updateSource (bagRemove 1 m)) t
     return Turn { manaDies   = manaDies - 1
                 , manaTokens = bagAdd 1 m manaTokens
                 , .. }

-- | Perform an exploration action.
payToExplore :: Turn -> Maybe Turn
payToExplore t@(Turn { .. }) =
  case turnPhase of
    TurnStart -> payToExplore
                  Turn { turnPhase = TurnMovement (newMovePhase turnTime), .. }
    TurnMovement MovePhase { mpMode = Walking } -> spendResource 2 Move t
    _                                           -> Nothing

-- | Pay the price for moving onto the given tile.
-- Assumes that the tile is actually accessible.
payToMove :: Terrain -> Turn -> Maybe Turn
payToMove terrain t0 =
  case turnPhase t0 of
    TurnStart -> payToMove terrain
                    t0 { turnPhase = TurnMovement (newMovePhase (turnTime t0)) }
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
      ReadyUnit n -> Text.concat [ "ready_unit_", Text.pack (show n) ]
      Influence   -> "influence"
      Attack t e  -> Text.concat [ "attack_", toKeyJS t, "_", toKeyJS e ]
      Block e     -> Text.append "block_" (toKeyJS e)

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

