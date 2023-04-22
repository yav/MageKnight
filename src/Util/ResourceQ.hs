module Util.ResourceQ
  ( ResourceQ
  , rqEmpty
  , rqFromListRandom
  , rqFromListOrdered
  , rqTake
  , rqDiscard
  ) where

import Common.RNGM

data ResourceQ a = ResourceQ
  { qAvailable   :: [a]
  , qDiscarded   :: [a]
  , qRandom      :: RNG
  }


rqEmpty :: Gen (ResourceQ a)
rqEmpty =
  do qRandom <- splitRNG
     return ResourceQ { qAvailable = [], qDiscarded = [], .. }

rqFromListRandom :: [a] -> Gen (ResourceQ a)
rqFromListRandom qDiscarded =
  do qRandom <- splitRNG
     return ResourceQ { qAvailable = [], .. }

rqFromListOrdered :: [a] -> Gen (ResourceQ a)
rqFromListOrdered qAvailable =
  do qRandom <- splitRNG
     return ResourceQ { qDiscarded = [], .. }

rqTake :: ResourceQ a -> Maybe (a, ResourceQ a)
rqTake ResourceQ { .. } =
  case qAvailable of
    a : as -> Just (a, ResourceQ { qAvailable = as, .. })
    [] -> case qDiscarded of
            [] -> Nothing
            _  -> Just $ let (a:as, g) = runRNG qRandom (shuffle qDiscarded)
                         in (a, ResourceQ { qAvailable = as
                                          , qDiscarded = []
                                          , qRandom    = g })

rqDiscard :: a -> ResourceQ a -> ResourceQ a
rqDiscard a ResourceQ { .. } = ResourceQ { qDiscarded = a : qDiscarded, .. }



