module MageKnight.Game where

import MageKnight.Common
import MageKnight.Bag
import MageKnight.Units
import MageKnight.Terrain
import MageKnight.Enemies
import MageKnight.Ruins
import MageKnight.Cards

import Data.Map (Map)
import Data.Text (Text)

data MageKnight = MageKnight
  { mkName        :: Text
  , mkFame        :: Int
  , mkReputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , mkArmor       :: Int
  , mkCardLimit   :: Int
  , mkUnits       :: [ Maybe Unit ]
  , mkCrystals    :: Bag BasicMana
  , mkDeedDeck    :: Bag CardName
  , mkHand        :: Bag CardName
  , mkDiscardPile :: Bag CardName
  }

instance Eq MageKnight where
  x == y = mkName x == mkName y

instance Ord MageKnight where
  compare x y = compare (mkName x) (mkName y)


data ResourceQ a = ResourceQ
  { available     :: [a]
  , forRecycling  :: [a]
  }

data Offer = Offer
  { offerDeck :: ResourceQ Card
  , offering  :: [Card]
  }

data Offers = Offers
  { avancedActionOffer :: Offer
  , spellOffer         :: Offer
  , unitOffer          :: Offer
  , monasteryTech      :: [CardName]
  , artifactDeck       :: ResourceQ Card
  }

type PlayerId = Int

data Land = Land
  { landTiles       :: Map TileAddr Tile
  , landPlayers     :: Map Addr MageKnight
  , landShields     :: Map Addr (Bag PlayerId)
  , landEnemies     :: Map Addr (Visibility, Bag Enemy)
  , landRuins       :: Map Addr (Visibility, Ruins)
  , unexploredTiles :: [ Tile ]
  , unexploredRuins :: ResourceQ Ruins
  }

data Visibility = Revealed | Hidden


data Game = Game
  { gameTime  :: Time
  , theSource :: Bag Mana
  , offers    :: Offers
  , enemyPool :: Map EnemyType (ResourceQ Enemy)
  , land      :: Land
  }

