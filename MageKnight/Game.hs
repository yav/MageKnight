module MageKnight.Game where

import MageKnight.Common
import MageKnight.Bag
import MageKnight.Units
import MageKnight.Terrain
import MageKnight.GameTile
import MageKnight.Enemies
import MageKnight.Ruins
import MageKnight.Cards
import           MageKnight.ResourceQ ( ResourceQ )
import qualified MageKnight.ResourceQ as RQ

import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Random (StdGen, split)


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










data Land = Land
  { landTiles       :: Map TileAddr GameTile
  , unexploredTiles :: [ Tile ]
  , unexploredRuins :: ResourceQ Ruins
  }


data Game = Game
  { gameTime  :: Time
  , theSource :: Bag Mana
  , offers    :: Offers
  , enemyPool :: Map EnemyType (ResourceQ Enemy)
  , land      :: Land
  }


blankEmenyPool :: StdGen -> Map EnemyType (ResourceQ Enemy)
blankEmenyPool g0 = snd $ foldr add (g0,Map.empty) allEnemyTypes
  where
  add e (g,m) = let (g1,g2) = split g
                in (g2, Map.insert e (RQ.empty g1) m)

initialEnemyPool :: StdGen -> Map EnemyType (ResourceQ Enemy)
initialEnemyPool g0 = foldr add (blankEmenyPool g0) allEnemies
  where
  add e qs = Map.adjust (RQ.discard e) (enemyType e) qs




