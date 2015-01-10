module MageKnight.Bag where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )

-- Invariant: the amount of each resource is strictly positive.

newtype Bag a = Bag (Map a Int)

bagEmpty :: Bag a
bagEmpty = Bag Map.empty

bagAdd :: Ord a => Int -> a -> Bag a -> Bag a
bagAdd q r (Bag m)
  | q > 0     = Bag (Map.insertWith (+) r q m)
  | otherwise = Bag m

bagSingleton :: Ord a => a -> Bag a
bagSingleton a = bagAdd 1 a bagEmpty

bagUnion :: Ord a => Bag a -> Bag a -> Bag a
bagUnion (Bag a) (Bag b) = Bag (Map.unionWith (+) a b)

bagRemove :: Ord a => Int -> a -> Bag a -> Maybe (Bag a)
bagRemove q r (Bag m)
  | avail >= q  = Just (Bag m1)
  | otherwise   = Nothing
  where
  upd _ qCur  = if qCur > q then Just (qCur - q) else Nothing
  (mbCur, m1) = Map.updateLookupWithKey upd r m
  avail       = fromMaybe 0 mbCur

bagLookup :: Ord a => a -> Bag a -> Int
bagLookup r (Bag m) = Map.findWithDefault 0 r m

bagToListGrouped :: Bag a -> [(a,Int)]
bagToListGrouped (Bag m) = Map.toList m

bagToList :: Bag a -> [a]
bagToList = flatGrouped . bagToListGrouped

bagFromList :: Ord a => [a] -> Bag a
bagFromList = foldr (bagAdd 1) bagEmpty

flatGrouped :: [(a,Int)] -> [a]
flatGrouped xs = [ x | (a,n) <- xs, x <- replicate n a ]


