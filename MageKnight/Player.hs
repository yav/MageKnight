module MageKnight.Player where

import MageKnight.Common
import MageKnight.Cards
import MageKnight.Bag
import MageKnight.Units

import           Data.Text (Text)

data Player = Player
  { playerName        :: Text
  , playerFame        :: Int
  , playerReputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , playerArmor       :: Int
  , playerCardLimit   :: Int
  , playerUnits       :: [ Maybe Unit ]
  , playerCrystals    :: Bag BasicMana
  , playerDeedDeck    :: [ Card ]
  , playerHand        :: Bag Card
  , playerDiscardPile :: [ Card ]
  }

instance Eq Player where
  x == y = playerName x == playerName y

instance Ord Player where
  compare x y = compare (playerName x) (playerName y)





