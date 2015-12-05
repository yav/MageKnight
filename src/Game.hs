module Game where

import Player
import Land
import Offers
import Source

data Game = Game
  { gamePlayers :: [Player] -- ^ In round order
  , gameLand    :: Land
  , gameOffers  :: Offers
  , gameSource  :: Source
  }


