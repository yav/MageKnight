-- | A draw deck together with some items on display for sale.
module Offer
  ( Offer
  , offerNew

    -- * The items on offer
  , offerNewItem
  , offerNewItems
  , offerClear
  , offerTakeItem
  , offerTakeLastItem
  , offerItems
  , offerLength

    -- * The draw deck
  , offerTakeFromDraw
  , offerTakeManyFromDraw
  , offerReturnItem
  , offerReturnMany

  ) where

import Common.RNGM
import Util.ResourceQ

data Offer a = Offer
  { offerDeck :: ResourceQ a  -- ^ New offers come from here
  , offering  :: [a]          -- ^ Latest offers at the front
  }

-- | A blank offer that will serve the given items.
offerNew :: [a] -> Gen (Offer a)
offerNew is =
  do offerDeck <- rqFromListRandom is
     return Offer { offering  = [], .. }

-- | Draw an item from the deck, and add it to the offerings.
-- Reshuffle the discard if needed.
-- Do nothing if there is nothintg in the draw pile or the discard.
offerNewItem :: Offer a -> Offer a
offerNewItem Offer { .. } =
  case rqTake offerDeck of
    Just (c,d) -> Offer { offerDeck = d, offering = c : offering }
    Nothing    -> Offer { .. }

-- | Draw multiple items to add to the offer.
offerNewItems :: Int -> Offer a -> Offer a
offerNewItems n o = iterate offerNewItem o !! n

-- | Discard everythign that's on offer.
offerClear :: Offer a -> Offer a
offerClear Offer { .. } = Offer
  { offering = []
  , offerDeck = foldr rqDiscard offerDeck offering
  }

-- | Take one of the offered items, identified positionally in the offerings.
offerTakeItem :: Int -> Offer a -> Maybe (a, Offer a)
offerTakeItem n Offer { .. } =
  case splitAt n offering of
    (as,b:bs) -> pure (b, Offer { offering = as ++ bs, .. })
    _         -> Nothing

-- | Take the last item in the offer.
offerTakeLastItem :: Offer a -> Maybe (a, Offer a)
offerTakeLastItem Offer { .. } =
  case reverse offering of
    x : xs -> pure (x, Offer { offering = reverse xs, .. })
    []      -> Nothing

-- | Take an item from the draw pile.  If it is empty, reshufle discard.
offerTakeFromDraw :: Offer a -> Maybe (a, Offer a)
offerTakeFromDraw Offer { .. } =
  do (a,as) <- rqTake offerDeck
     pure (a, Offer { offerDeck = as, .. })

-- | Draw the given number of cards from the draw pile.
-- If the draw pile runs out reshuffle.
-- May return a shorter list, if both the draw and the discard run out.
offerTakeManyFromDraw :: Int -> Offer a -> ([a], Offer a)
offerTakeManyFromDraw = go []
  where
  go as n o
    | n > 0 =
      case offerTakeFromDraw o of
        Just (a,o1) -> go (a:as) (n-1) o1
        Nothing     -> done as o

    | otherwise = done as o

  done as o = (reverse as, o)

-- | Add an item to the discard pile.
offerReturnItem :: a -> Offer a -> Offer a
offerReturnItem a Offer { .. } =
  Offer { offerDeck = rqDiscard a offerDeck, .. }

-- | Add multiple items to discard pile
offerReturnMany :: [a] -> Offer a -> Offer a
offerReturnMany as o = foldr offerReturnItem o as

-- | How many items are on offer.
offerLength :: Offer a -> Int
offerLength = length . offerItems

-- | What's on offer
offerItems :: Offer a -> [a]
offerItems = offering



