{-# LANGUAGE Safe, RecordWildCards #-}
module MageKnight.ResourceQ
  ( ResourceQ
  , empty
  , MageKnight.ResourceQ.take
  , discard
  ) where

import MageKnight.Random

data ResourceQ a = ResourceQ
  { qAvailable   :: [a]
  , qDiscarded   :: [a]
  , qRandom      :: StdGen
  }


empty :: StdGen -> ResourceQ a
empty qRandom = ResourceQ { qAvailable = [], qDiscarded = [], .. }

take :: ResourceQ a -> Maybe (a, ResourceQ a)
take ResourceQ { .. } =
  case qAvailable of
    a : as -> Just (a, ResourceQ { qAvailable = as, .. })
    []     -> case qDiscarded of
                [] -> Nothing
                _  -> Just $ let (a:as, g) = shuffle qRandom qDiscarded
                             in (a, ResourceQ { qAvailable = as
                                              , qDiscarded = []
                                              , qRandom    = g })

discard :: a -> ResourceQ a -> ResourceQ a
discard a ResourceQ { .. } = ResourceQ { qDiscarded = a : qDiscarded, .. }

