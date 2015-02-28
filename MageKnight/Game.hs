{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module MageKnight.Game where

import           MageKnight.Offers
import           MageKnight.Source
import           MageKnight.Land
import           MageKnight.Random
import           MageKnight.Terrain
import           MageKnight.JSON
import           MageKnight.Player
import           MageKnight.DeedDecks(makeCustomDeck, arytheaDeck)




testGame :: StdGen -> Game
testGame g =
  Game { theSource  = newSource sourceRNG 3
       , offers     = iterate newMonastery offers0 !! ms
       , theLand    = placePlayer pl l
       , player     = pl
       }
  where
  offers0     = setupOffers offerRNG (defaultOfferSetup 1 True)
  Just (l,ms) = setupLand landRNG (defaultLandSetup Wedge 7 2 [3,5])
  (offerRNG, g1)     = split g
  (landRNG,sourceRNG) = split g1
  pl = newPlayer "Arythea" (makeCustomDeck arytheaDeck)




data Game = Game
  { theSource   :: Source
  , offers      :: Offers
  , theLand     :: Land
  , player      :: Player   -- just one for now
  }

updatePlayer :: Functor f => (Player -> f Player) -> Game -> f Game
updatePlayer f Game { .. } = fmap (\p1 -> Game { player = p1, .. }) (f player)

updateSource :: Functor f => (Source -> f Source) -> Game -> f Game
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
      [ "source" .= theSource
      , "offers" .= offers
      , "land"   .= theLand
      , "player" .= player
      ]


