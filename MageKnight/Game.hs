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
import           MageKnight.DeedDecks(makeCustomDeck, arytheaDeck)




testGame :: StdGen -> Game
testGame g =
  Game { theSource  = bagFromList [ BasicMana Red, BasicMana Green, Gold ]
       , offers     = iterate newMonastery offers0 !! ms
       , theLand    = placePlayer pl l
       , player     = pl
       , gameRNG    = gameRand
       }
  where
  offers0     = setupOffers offerRand (defaultOfferSetup 1 True)
  Just (l,ms) = setupLand landRand (defaultLandSetup Wedge 7 2 [3,5])
  (offerRand, g1)     = split g
  (landRand,gameRand) = split g1
  pl = newPlayer "Arythea" (makeCustomDeck arytheaDeck)




data Game = Game
  { theSource   :: Bag Mana
  , offers      :: Offers
  , theLand     :: Land
  , player      :: Player   -- just one for now
  , gameRNG     :: StdGen
  }

updatePlayer :: Functor f => (Player -> f Player) -> Game -> f Game
updatePlayer f Game { .. } = fmap (\p1 -> Game { player = p1, .. }) (f player)

updateSource :: Functor f => (Bag Mana -> f (Bag Mana)) -> Game -> f Game
updateSource f Game { .. } = fmap (\p1 -> Game { theSource = p1, .. })
                                  (f theSource)

-- | Move the player 1 unit the given direction.
movePlayer :: Dir -> Game -> Game
movePlayer d Game { .. } =
  let (p1,l1) = MageKnight.Land.movePlayer player d theLand
  in Game { theLand = l1, player = p1, .. }


addrOnMap :: Addr -> Game -> Bool
addrOnMap a g = MageKnight.Land.addrOnMap a (theLand g)


-- | The player tries to explore the given address.
explore :: Addr -> Game -> Maybe Game
explore addr g0 =
  -- XXX: check distance
  do (l,ms) <- exploreAt (playerLocation (player g0))
                         (addrGlobal addr)
                         (theLand g0)
     let addMon g = g { offers = newMonastery (offers g) }
     return (iterate addMon g0 { theLand = l } !! ms)


--------------------------------------------------------------------------------

instance Export Game where
  toJS Game { .. } =
    object
      [ "source" .= bagToList theSource
      , "offers" .= offers
      , "land"   .= theLand
      , "player" .= player
      ]


