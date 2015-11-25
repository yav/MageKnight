{-# LANGUAGE Safe, RecordWildCards #-}
module Util.Q
  ( Q
  , qEmpty
  , qFromList
  , qToList
  , qAddFront
  , qAddBack
  , qTakeFront
  , qTakeFrontN
  ) where

-- A queue with amorthazied O(1) access to front and back.
data Q a = Q { front :: [a], back :: [a] }

-- | An empty queue.
qEmpty :: Q a
qEmpty = Q { front = [], back = [] }

-- | A queue that contains the elements in a list.
qFromList :: [a] -> Q a
qFromList xs = Q { front = xs, back = [] }

qToList :: Q a -> [a]
qToList Q { .. } = front ++ reverse back

-- | Add an element to the front of teh queue.
qAddFront :: a -> Q a -> Q a
qAddFront a Q { .. } = Q { front = a : front, .. }

-- | Add an element to the end of the quque
qAddBack :: a -> Q a -> Q a
qAddBack x Q { .. } = Q { back = x : back, .. }

-- | Take an element from the front of the quque, if any.
qTakeFront :: Q a -> Maybe (a, Q a)
qTakeFront Q { .. } =
  case front of
    a : as -> Just (a, Q { front = as, .. })
    []     -> case reverse back of
                a : as -> Just (a, Q { front = as, back = [] })
                []     -> Nothing

qTakeFrontN :: Int -> Q a -> Maybe ([a], Q a)
qTakeFrontN n q = if n > 0 then do (a,q1) <- qTakeFront q
                                   (as,q2) <- qTakeFrontN (n-1) q1
                                   return (a:as,q2)
                           else return ([], q)



