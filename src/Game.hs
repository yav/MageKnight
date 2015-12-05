module Game where

import Player
import Land
import Offers
import Source

data Game = Game
  { gamePlayer  :: Player
  , gameLand    :: Land
  , gameOffers  :: Offers
  , gameSource  :: Source
  }


