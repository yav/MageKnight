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
  , explore

  ) where

import MageKnight.Common ( Element(..), AttackType(..), Mana(..)
                         , BasicMana(..), Time(..)
                         )
import MageKnight.Source
import MageKnight.Offers
import MageKnight.Terrain
import MageKnight.Game
import MageKnight.Player
import MageKnight.Land
import MageKnight.Bag
import MageKnight.JSON
import MageKnight.Movement
import MageKnight.Perhaps

import qualified Data.Text as Text
import qualified Data.Set  as Set



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
useManaDie :: Mana -> Turn -> Perhaps Turn
useManaDie m t =
  do checkThat (manaDies t >= 1) "You don't have access to the source."
     case turnTime t of
       Day   -> checkThat (m /= Black)
                  "Black mana is not available during the day."
       Night -> checkThat (m /= Gold)
                  "Gold mana is not available during the night."
     Turn { .. } <- perhaps "The source is empty."
                  $ updateGame (updateSource (takeMana m)) t
     return Turn { manaDies   = manaDies - 1
                 , manaTokens = bagAdd 1 m manaTokens
                 , .. }

-- | Perform an exploration action.
explore :: Addr -> Turn -> Perhaps Turn
explore a t =
  case turnPhase t of
    TurnStart -> explore a
                      t { turnPhase = TurnMovement (newMovePhase (turnTime t)) }

    TurnMovement mp0 ->
      do mp <- perhaps "The current movement mode does not allow exploration."
               $ useWalking mp0
         t1 <- perhaps "Insufficient movement points."
               $ spendResource 2 Move t
         let g   = turnGame t1
             loc = playerLocation (player g)
             land = theLand g
         checkThat (a `Set.member` neighboursUpTo (oneMoveLimit mp) loc)
                   "The location is too far away."
         (newLand, newMons) <- exploreAt loc (addrGlobal a) land
         let g1 = g { theLand = newLand
                    , offers  = iterate newMonastery (offers g) !! newMons
                    }
         return t1 { turnGame = g1 }


    _ -> fail "Exploration is available only during the movement phase."

{-
-- | Perform a movement.
move :: Addr -> Turn -> Perhaps Turn
move a t0 =
  case turnPhase t0 of
    TurnStart -> move a
                    t0 { turnPhase = TurnMovement (newMovePhase (turnTime t0)) }
    TurnMovement mp ->
      do let g0   = turnGame t0
             loc0 = playerLocation (player g0)
         guard (a `Set.member` neighboursUpTo (mpRadius mp) loc0)
         undefined

      (c,mp1) <- tryToMove terrain mp
         t1      <- spendResource c Move t0
         return t1 { turnPhase = TurnMovement mp1 }
-}
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

