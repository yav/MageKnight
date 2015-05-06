{-# LANGUAGE RecordWildCards, OverloadedStrings, Trustworthy #-}
module MageKnight.Game where

import Debug.Trace


import           MageKnight.Common hiding (Resource(..))
import           MageKnight.Deed
import           MageKnight.Offers
import           MageKnight.Source
import           MageKnight.Land
import           MageKnight.Random
import           MageKnight.Terrain
import           MageKnight.JSON
import           MageKnight.Player
import           MageKnight.DeedDecks(makeCustomDeck, arytheaDeck)
import           MageKnight.Perhaps
import           MageKnight.Bag
import           MageKnight.Loc

import           Data.Text (Text)
import           Data.Maybe(fromMaybe)



testGame :: StdGen -> Game
testGame g =
  Game { manaSource = newSource sourceRNG 3
       , offers     = iterate newMonastery offers0 !! ms
       , land       = placePlayer pl l
       , player     = pl
       , playArea   = emptyPlayArea
       }
  where
  offers0     = setupOffers offerRNG (defaultOfferSetup 1 True)
  Ok (l,ms)   = setupLand landRNG (defaultLandSetup Wedge 7 2 [3,5])
  (offerRNG, g1)        = split g
  (landRNG,g2)          = split g1
  (playerRNG,sourceRNG) = split g2

  pl = (!! 3) $ iterate (addCrystal' Green)
      $ (!! 3) $ iterate (addCrystal' Red)
      $ (!! 1) $ iterate (addCrystal' White)
      $ (!! 2) $ iterate (addCrystal' Blue)
     $ newPlayer playerRNG "arythea" (makeCustomDeck arytheaDeck)

  addCrystal' x ga = fromMaybe ga (addCrystal x ga)


data Game = Game
  { manaSource  :: Source
  , offers      :: Offers
  , land        :: Land
  , player      :: Player   -- just one for now
  , playArea    :: PlayArea
  }

thePlayer :: MonoLoc Game Player
thePlayer = loc player (\g a -> g { player = a })

theSource :: MonoLoc Game Source
theSource = loc manaSource (\g a -> g { manaSource = a })

theOffers :: MonoLoc Game Offers
theOffers = loc offers (\g a -> g { offers = a })

theLand :: MonoLoc Game Land
theLand = loc land (\g a -> g { land = a })

thePlayArea :: MonoLoc Game PlayArea
thePlayArea = loc playArea (\g a -> g { playArea = a })


useCrystal :: BasicMana -> Game -> Game
useCrystal m Game { .. } =
  case removeCrystal m player of
    Just p1 -> Game { player = p1
                    , playArea = addManaToken (BasicMana m) playArea
                    , .. }
    Nothing -> Game { .. }

useDie :: Mana -> Game -> Maybe Game
useDie m Game { .. } =
  do s1 <- takeMana m manaSource
     return Game { manaSource = s1
                 , playArea = addManaToken m playArea
                 , .. }

gameRefillSource :: Game -> Game
gameRefillSource g = writeLoc g theSource refillSource

playCard :: Int -> Game -> Game
playCard n Game { .. } =
  case takeCard n player of
    Just (d,p1) -> Game { player = p1, playArea = addCard d playArea, .. }
    Nothing     -> Game { .. }

playCardFor :: ActionType -> Int -> Game -> Game
playCardFor a n Game { .. } =
  case takeCard n player of
    Just (d,p1) -> Game { player = p1
                        , playArea = addSidewaysCard a d playArea
                        , .. }
    Nothing     -> Game { .. }


{-
-- | Move the player 1 unit the given direction.
movePlayer :: Dir -> Game -> Game
movePlayer d Game { .. } =
  let (p1,l1) = MageKnight.Land.movePlayer player d theLand
  in Game { theLand = l1, player = p1, .. }
-}

addrOnMap :: Addr -> Game -> Bool
addrOnMap a g = MageKnight.Land.isRevealed a (land g)


{-
-- | The player tries to explore the given address.
explore :: Addr -> Game -> Maybe Game
explore addr g0 =
  -- XXX: check distance
  do (l,ms) <- exploreAt (playerLocation (player g0))
                         (addrGlobal addr)
                         (theLand g0)
     let addMon g = g { offers = newMonastery (offers g) }
     return (iterate addMon g0 { theLand = l } !! ms)
-}

--------------------------------------------------------------------------------

instance Export Game where
  toJS g =
    object
      [ "source"    .= readLoc g theSource
      , "offers"    .= readLoc g theOffers
      , "land"      .= readLoc g theLand
      , "player"    .= readLoc g thePlayer
      , "playArea"  .= readLoc g thePlayArea
      ]

--------------------------------------------------------------------------------

data PlayArea = PlayArea
  { manaTokens      :: Bag Mana
  , discardedCards  :: [Deed]
  , trashedCards    :: [Deed]
  , activeCards     :: [ActiveCard]
  }

data ActiveCard   = SidewaysCard ActionType Deed
                  | NormalCard Bool {- powered up? -} Deed

data ActionType   = Movement | Influence | Block | Attack


emptyPlayArea :: PlayArea
emptyPlayArea = PlayArea
  { manaTokens      = bagEmpty
  , discardedCards  = []
  , trashedCards    = []
  , activeCards     = []
  }

addManaToken :: Mana -> PlayArea -> PlayArea
addManaToken m PlayArea { .. } =
               PlayArea  { manaTokens = bagAdd 1 m manaTokens, .. }

removeManaToken :: Mana -> PlayArea -> PlayArea
removeManaToken m PlayArea { .. } =
  case bagRemove 1 m manaTokens of
    Just b  -> PlayArea { manaTokens = b, .. }
    Nothing -> PlayArea { .. }

addSidewaysCard :: ActionType -> Deed -> PlayArea -> PlayArea
addSidewaysCard a d PlayArea { .. } =
  PlayArea { activeCards = SidewaysCard a d : activeCards, .. }

addCard :: Deed -> PlayArea -> PlayArea
addCard d PlayArea { .. } =
  PlayArea { activeCards = NormalCard False d : activeCards, .. }

powerUpCard :: Int -> PlayArea -> PlayArea
powerUpCard n PlayArea { .. } =
  case splitAt n activeCards of
    (as,NormalCard _ d : bs) ->
        PlayArea { activeCards = as ++ NormalCard True d : bs, .. }
    _ -> trace "no" PlayArea { .. }

discardCard :: Deed -> PlayArea -> PlayArea
discardCard d PlayArea { .. } =
  PlayArea { discardedCards = d : discardedCards, .. }

trashCard :: Deed -> PlayArea -> PlayArea
trashCard d PlayArea { .. } =
  PlayArea { trashedCards = d : trashedCards, .. }



instance Export PlayArea where
  toJS PlayArea { .. } =
    object
      [ "mana"      .= bagToList manaTokens
      , "cards"     .= activeCards
      , "discarded" .= discardedCards
      , "trashed"   .= trashedCards
      ]

instance Export ActiveCard where
  toJS card =
    case card of
      SidewaysCard act d -> object [ "card" .= d, "useFor"  .= act ]
      NormalCard p d     -> object [ "card" .= d, "powerUp" .= p ]

instance Export ActionType where
  toJS act = toJS $ case act of
                      Movement  -> "move" :: Text
                      Influence -> "influence"
                      Block     -> "block"
                      Attack    -> "attack"






