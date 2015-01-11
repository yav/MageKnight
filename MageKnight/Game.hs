module MageKnight.Game where

import MageKnight.Common
import MageKnight.Bag
import MageKnight.Units

data MageKnight = MageKnight
  { mkFame        :: Int
  , mkReputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , mkArmor       :: Int
  , mkCardLimit   :: Int
  , mkUnits       :: [ Maybe Unit ]
  , mkCrystals    :: Bag BasicMana
  , mkDeedDeck    :: Bag CardName
  , mkHand        :: Bag CardName
  , mkDiscardPile :: Bag CardName
  }


data Offer = Offer
  { offerDeck :: Bag CardName
  , offering  :: Bag CardName
  }

data Offers = Offers
  { avancedActionOffer :: Offer
  , spellOffer         :: Offer
  , unitOffer          :: Offer
  , monasteryTech      :: Bag CardName
  , artifactDeck       :: Bag CardName
  }

data Game = Game
  { gameTime  :: Time
  , theSource :: Bag Mana
  , offers    :: Offers
  -- player
  -- map
  }

