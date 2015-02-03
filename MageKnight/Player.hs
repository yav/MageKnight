{-# LANGUAGE RecordWildCards #-}
module MageKnight.Player where

import MageKnight.Common
import MageKnight.Cards
import MageKnight.Units
import MageKnight.Bag
import MageKnight.Terrain

import           Data.Text (Text)

type PlayerName = Text

data Player = Player
  { playerName        :: PlayerName
  , playerFame        :: Int
  , playerReputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , playerArmor       :: Int
  , playerCardLimit   :: Int
  , playerUnits       :: [ Maybe Unit ]
  , playerCrystals    :: Bag BasicMana
  , playerDeedDeck    :: [ PlayerCard ]
  , playerHand        :: Bag PlayerCard
  , playerDiscardPile :: [ PlayerCard ]
  , playerLocation    :: Addr
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
  }


data PlayerCard = Wound | NormalCard Card
                  deriving (Eq,Ord)

instance Eq Player where
  x == y = playerName x == playerName y

instance Ord Player where
  compare x y = compare (playerName x) (playerName y)

assignDamage :: Int -> Player -> (Player,Bool)
assignDamage d Player { .. }
  | totalWounds >= playerCardLimit =
      (Player { playerHand        = bagAdd totalWounds Wound bagEmpty
              , playerDiscardPile = bagToList (bagRemoveAll Wound hand1)
              , ..
              }, True)

  | otherwise =
      ( Player { playerHand = hand1, .. }
      , False
      )
  where
  woundNum    = div (max 0 d + playerArmor - 1) playerArmor
  hand1       = bagAdd woundNum Wound playerHand
  totalWounds = bagLookup Wound hand1




