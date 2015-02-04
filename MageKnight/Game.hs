{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module MageKnight.Game where

import           MageKnight.Common
import           MageKnight.Offers
import           MageKnight.Land
import           MageKnight.Random
import           MageKnight.Bag
import           MageKnight.Terrain
import           MageKnight.JSON
import           MageKnight.Player




testGame :: StdGen -> Game
testGame g =
  Game { theSource  = bagFromList [ BasicMana Red, BasicMana Green, Gold ]
       , offers     = iterate newMonastery offers0 !! ms
       , theLand    = placePlayer pl l
       , player     = pl
       }
  where
  offers0     = setupOffers offerRand (defaultOfferSetup 1 True)
  Just (l,ms) = setupLand landRand (defaultLandSetup Wedge 7 2 [3,5])
  (offerRand,landRand) = split g
  pl = newPlayer "arythea"




data Game = Game
  { theSource   :: Bag Mana
  , offers      :: Offers
  , theLand     :: Land
  , player      :: Player
  }

movePlayer :: Dir -> Game -> Game
movePlayer d Game { .. } =
  let (p1,l1) = MageKnight.Land.movePlayer player d theLand
  in Game { theLand = l1, player = p1, .. }


addrOnMap :: Addr -> Game -> Bool
addrOnMap a g = MageKnight.Land.addrOnMap a (theLand g)


-- | Try to explore from the given position in the given direction.
explore :: Addr -> Dir -> Game -> Maybe Game
explore addr dir g0 =
  do (l,ms) <- exploreInDir addr dir (theLand g0)
     let addMon g = g { offers = newMonastery (offers g) }
     return (iterate addMon g0 { theLand = l } !! ms)


--------------------------------------------------------------------------------

instance Export Game where
  toJS Game { .. } =
    object
      [ "source" .= bagToList theSource
      , "offers" .= offers
      , "land"   .= theLand
      , "focus"  .= object [ "x" .= x, "y" .= y ]
      ]
    where (x,y) = addrGlobal (playerLocation player)


