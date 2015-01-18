{-# LANGUAGE RecordWildCards #-}
module MageKnight.Offers where

import           MageKnight.ResourceQ (ResourceQ)
import qualified MageKnight.ResourceQ as RQ
import           MageKnight.Random
import           MageKnight.Units
import           MageKnight.Cards

data Offer a = Offer
  { offerDeck :: ResourceQ a
  , offering  :: [a]  -- newest first
  }

-- | A blank offer that will serve the given items.
offerNothing :: StdGen -> [a] -> Offer a
offerNothing r is = Offer { offerDeck = RQ.fromListRandom r is, offering  = [] }

-- | Draw an item from the deck, and add it to the offerings.
offerNewItem :: Offer a -> Offer a
offerNewItem Offer { .. } =
  case RQ.take offerDeck of
    Just (c,d) -> Offer { offerDeck = d, offering = c : offering }
    Nothing    -> Offer { .. }

-- | Draw multiple items to add to the offer.
offerNewItems :: Int -> Offer a -> Offer a
offerNewItems n o = iterate offerNewItem o !! n

-- | Remove everything that's on offer, and return it to the deck.
offerClear :: Offer a -> Offer a
offerClear Offer { .. } = Offer
  { offering = []
  , offerDeck = foldr RQ.discard offerDeck offering
  }

-- | Take one of the offered items.  The index is ths position.
offerTakeItem :: Int -> Offer a -> Maybe (a, Offer a)
offerTakeItem n0 Offer { .. }
  | n0 >= 0    = do (c,os) <- go n0 offering
                    return (c, Offer { offering = os, .. })
  | otherwise = Nothing
  where
  go _ []       = Nothing
  go 0 (c : cs) = Just (c, cs)
  go n (c : cs) = do (a,as) <- go (n-1) cs
                     return (a, c : as)

-- | Add an item back to the offer's deck.
offerReturnItem :: a -> Offer a -> Offer a
offerReturnItem a Offer { .. } =
  Offer { offerDeck = RQ.discard a offerDeck, .. }

-- | How many items are on offer.
offeringLength :: Offer a -> Int
offeringLength Offer { .. } = length offering


--------------------------------------------------------------------------------

newCardOffer :: StdGen -> [Card] -> Offer Card
newCardOffer r cs = offerNewItems 3 (offerNothing r cs)

refreshCardOffer :: Offer Card -> Maybe (Offer Card)
refreshCardOffer Offer { .. } =
  case reverse offering of
    [] -> Nothing
    x : xs -> Just $ offerNewItem
                   $ offerReturnItem x
                     Offer { offering  = reverse xs, .. }

refreshCardOfferDummy :: Offer Card -> Maybe (Card, Offer Card)
refreshCardOfferDummy Offer { .. } =
  case reverse offering of
    [] -> Nothing
    x : xs -> Just (x, offerNewItem Offer { offering = reverse xs, .. })

offerTakeCard :: Int -> Offer Card -> Maybe (Card, Offer Card)
offerTakeCard n o =
  do (c,o1) <- offerTakeItem n o
     return (c, offerNewItem o1)

offerReturnCards :: [Card] -> Offer Card -> Offer Card
offerReturnCards cs o = foldr offerReturnItem o cs

--------------------------------------------------------------------------------

data UnitOffer = UnitOffer
  { regularUnitOffer :: Offer Unit
  , eliteUnitOffer   :: Offer Unit
  }

unitsOnOffer :: UnitOffer -> [Unit]
unitsOnOffer UnitOffer { .. } = offering regularUnitOffer ++
                                offering eliteUnitOffer

emptyUnitOffer :: StdGen -> [Unit] -> [Unit] -> UnitOffer
emptyUnitOffer g regular elite = UnitOffer
  { regularUnitOffer = offerNothing g1 regular
  , eliteUnitOffer   = offerNothing g2 elite
  }
  where
  (g1,g2) = split g

refreshOnlyRegulars :: Int -> UnitOffer -> UnitOffer
refreshOnlyRegulars totalNum UnitOffer { .. } = UnitOffer
  { regularUnitOffer = offerNewItems totalNum
                                      (offerClear regularUnitOffer)
  , eliteUnitOffer   = offerClear eliteUnitOffer
  }

refreshWithElite :: Int -> UnitOffer -> UnitOffer
refreshWithElite totalNum UnitOffer { .. } = UnitOffer
  { regularUnitOffer = offerNewItems regularNum
                                      (offerClear regularUnitOffer)
  , eliteUnitOffer   = offerNewItems eliteNum
                                      (offerClear eliteUnitOffer)
  }
  where
  regularNum = div totalNum 2
  eliteNum   = totalNum - regularNum

offerTakeUnit :: Int -> UnitOffer -> Maybe (Unit, UnitOffer)
offerTakeUnit n UnitOffer { .. }
  | n < 0 = Nothing
  | Just (u,rs) <- offerTakeItem n regularUnitOffer =
                        Just (u, UnitOffer { regularUnitOffer = rs, .. })
  | otherwise =
    do let eliteIx = n - offeringLength regularUnitOffer
       (u,es) <- offerTakeItem eliteIx eliteUnitOffer
       return (u, UnitOffer { eliteUnitOffer = es, .. })

offerDisbandUnit :: Unit -> UnitOffer -> UnitOffer
offerDisbandUnit u UnitOffer { .. } =
  case unitType u of
    RegularUnit ->
      UnitOffer { regularUnitOffer = offerReturnItem u regularUnitOffer, ..}
    EliteUnit ->
      UnitOffer { eliteUnitOffer = offerReturnItem u eliteUnitOffer, .. }

--------------------------------------------------------------------------------

