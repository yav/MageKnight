module MageKnight.Game1 where

import MageKnight.Player
import MageKnight.Land
import MageKnight.Offers
import MageKnight.Source

data Game = Game
  { gamePlayers :: [Player] -- ^ In round order
  , gameLand    :: Land
  , gameOffers  :: Offers
  , gameSource  :: Source
  }


