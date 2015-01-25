{-# LANGUAGE RecordWildCards #-}
module MageKnight.Game where

import           MageKnight.Common
import           MageKnight.Offers
import           MageKnight.Land
import           MageKnight.Random
import           MageKnight.Bag
import           MageKnight.Terrain




testGame :: StdGen -> Game
testGame g =
  Game { theSource  = bagFromList [ BasicMana Red, BasicMana Green, Gold ]
       , offers     = iterate newMonastery offers0 !! ms
       , theLand    = l
       }
  where
  offers0     = setupOffers offerRand (defaultOfferSetup 1 True)
  Just (l,ms) = setupLand landRand (defaultLandSetup Wedge 7 2 [3,5])
  (offerRand,landRand) = split g




data Game = Game
  { theSource   :: Bag Mana
  , offers      :: Offers
  , theLand     :: Land
  }



-- | Try to explore from the given position in the given direction.
explore :: Addr -> Dir -> Game -> Maybe Game
explore addr dir g0 =
  do (l,ms) <- exploreInDir addr dir (theLand g0)
     let addMon g = g { offers = newMonastery (offers g) }
     return (iterate addMon g0 { theLand = l } !! ms)



