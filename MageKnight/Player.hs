{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module MageKnight.Player where

import MageKnight.Common
import MageKnight.Cards
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
  , playerDeedDeck    :: [ PlayerCard ]
  , playerHand        :: Bag PlayerCard
  , playerDiscardPile :: [ PlayerCard ]
  , playerLocation    :: Addr

  , playerOnUnsafe    :: Maybe (Addr, Int)
    -- ^ `Nothing`, if we are currently safe.
    -- If unsafe, then last safe location, and how many wounds to get there.
  }


newPlayer :: Text -> Player
newPlayer name = Player
  { playerName        = name
  , playerFame        = 0
  , playerReputation  = 0
  , playerArmor       = 2
  , playerCardLimit   = 5
  , playerUnits       = [ Nothing ]
  , playerCrystals    = bagEmpty
  , playerDeedDeck    = []
  , playerHand        = bagEmpty
  , playerDiscardPile = []
  , playerLocation    = Addr { addrGlobal = (0,0), addrLocal = Center }

  , playerOnUnsafe    = Nothing
  }


data PlayerCard = Wound | NormalCard Card
                  deriving (Eq,Ord)

instance Eq Player where
  x == y = playerName x == playerName y

instance Ord Player where
  compare x y = compare (playerName x) (playerName y)


-- | Check if this player passed out.
-- The blloean indicates if the player passed out.
checkPassOut :: Player -> (Bool, Player)
checkPassOut Player { .. }
  | wounds >= playerCardLimit =
      ( True, Player
                { playerHand = bagAdd wounds Wound bagEmpty
                , playerDiscardPile = bagToList (bagRemoveAll Wound playerHand)
                , ..
                } )
  | otherwise = (False, Player { .. })
  where
  wounds = bagLookup Wound playerHand

-- | Assign combat damage to a player.  Returns 'True' if the player passed out.
assignDamage :: Int -> Player -> (Bool, Player)
assignDamage d p = checkPassOut
                      p { playerHand = bagAdd woundNum Wound (playerHand p) }
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
                                 , playerHand = bagAdd w Wound (playerHand p)
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
      , "unsafe"      .= (case playerOnUnsafe of
                            Nothing -> jsNull
                            Just (a,w) -> object [ "safe"   .= a
                                                 , "wounds" .= w
                                                 ])
      ]

instance Export PlayerCard where
  toJS c = toJS (case c of
                   Wound        -> "wound"
                   NormalCard x -> cardName x)


