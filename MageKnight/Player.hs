{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module MageKnight.Player where

import MageKnight.Common
import MageKnight.Deed
import MageKnight.Units
import MageKnight.Bag
import MageKnight.Terrain
import MageKnight.JSON

import           Data.Text (Text)

type PlayerName = Text

data Player = Player
  { playerName        :: PlayerName
  , playerFame        :: Int
  , playerReputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , playerArmor       :: Int
  , playerCardLimit   :: Int
  , playerUnits       :: [ Maybe Unit ]   -- XXX: Replace With active units
  , playerCrystals    :: Bag BasicMana
  , playerDeedDeck    :: [ Deed ]
  , playerHand        :: Bag Deed
  , playerDiscardPile :: [ Deed ]
  , playerLocation    :: Addr

  , playerOnUnsafe    :: Maybe (Addr, Int)
    -- ^ `Nothing`, if we are currently safe.
    -- If unsafe, then last safe location, and how many wounds to get there.
  }


newPlayer :: Text -> [Deed] -> Player
newPlayer name deeds = Player
  { playerName        = name
  , playerFame        = 0
  , playerReputation  = 0
  , playerArmor       = 2
  , playerCardLimit   = 5
  , playerUnits       = [ Nothing ]
  , playerCrystals    = bagEmpty
  , playerDeedDeck    = deeds
  , playerHand        = bagEmpty
  , playerDiscardPile = []
  , playerLocation    = Addr { addrGlobal = (0,0), addrLocal = Center }

  , playerOnUnsafe    = Nothing
  }


instance Eq Player where
  x == y = playerName x == playerName y

instance Ord Player where
  compare x y = compare (playerName x) (playerName y)


-- | Check if this player passed out.
-- The blloean indicates if the player passed out.
checkPassOut :: Player -> (Bool, Player)
checkPassOut Player { .. }
  | woundNum >= playerCardLimit =
      ( True, Player
                { playerHand = bagAdd woundNum wound bagEmpty
                , playerDiscardPile = bagToList (bagRemoveAll wound playerHand)
                , ..
                } )
  | otherwise = (False, Player { .. })
  where
  woundNum = bagLookup wound playerHand

-- | Assign combat damage to a player.  Returns 'True' if the player passed out.
assignDamage :: Int -> Player -> (Bool, Player)
assignDamage d p = checkPassOut
                      p { playerHand = bagAdd woundNum wound (playerHand p) }
  where
  armor       = playerArmor p
  woundNum    = div (max 0 d + armor - 1) armor


-- | Move the player back to their last safe location.
-- Return 'True' if the player passed out in the process.
backToSafety :: Player -> (Bool, Player)
backToSafety p =
  case playerOnUnsafe p of
    Nothing    -> (False, p)
    Just (a,w) -> checkPassOut p { playerLocation = a
                                 , playerHand = bagAdd w wound (playerHand p)
                                 }

instance Export Player where
  toJS Player { .. } =
    object
      [ "name"        .= playerName
      , "fame"        .= playerFame
      , "reputation"  .= playerReputation
      , "armor"       .= playerArmor
      , "cardLimit"   .= playerCardLimit
      , "units"       .= playerUnits
      , "crystals"    .= bagToList playerCrystals
      , "cards"       .= bagToList playerHand
      , "location"    .= playerLocation
      , "unsafe"      .= fmap fst playerOnUnsafe
      ]



