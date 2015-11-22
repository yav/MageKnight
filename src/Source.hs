{-# LANGUAGE RecordWildCards #-}
module Source
  ( Source
  , newSource
  , refillSource
  , renewSource

  , takeMana
  , stealMana
  , returnStolenMana
  , advancedManaDraw
  ) where

import Common
import Util.Bag
import Util.Random
import Util.JSON

import Control.Monad (guard, replicateM)

data Source = Source
  { sourceMana    :: Bag Mana
  , sourceRNG     :: StdGen
  , sourceSize    :: Int
  , sourceFixed   :: Bag Mana
  }


rollDice :: Int -> StdGen -> ([Mana], StdGen)
rollDice n g = genRand g $ replicateM n $ oneOf anyMana

-- | Make a new source, with the given number of dice.
newSource :: StdGen -> Int -> Source
newSource r0 sourceSize =
  let (sourceMana,sourceRNG) = mkSource r0
  in Source { sourceFixed = bagEmpty, .. }
  where
  validBag x = (bagLookup Gold x + bagLookup Black x) <= div sourceSize 2
  mkSource g = let (ds,g1) = rollDice sourceSize g
                   b       = bagFromList ds
               in if validBag b then (b,g1) else mkSource g1


-- | Replenish spent mana.  Used at end of turn.
refillSource :: Source -> Source
refillSource Source { .. } =
  Source { sourceRNG = rng
         , sourceMana = bagUnion (bagFromList ds) currentDice
         , sourceFixed = bagEmpty
         , ..
         }
  where
  currentDice = bagUnion sourceFixed sourceMana
  (ds, rng)   = rollDice (sourceSize - bagSize currentDice) sourceRNG

-- | Setup a whole new source.  Used at the end of a round.
-- Assumes that stolen mana has been returned.
renewSource :: Source -> Source
renewSource Source { .. } = newSource sourceRNG sourceSize



-- | Try to take some mana from the source.
takeMana :: Mana -> Source -> Maybe Source
takeMana m Source { .. } =
  do b <- bagRemove 1 m sourceMana
     return Source { sourceMana = b, .. }

-- | Take some mana from the source, and reduce its size.
-- Used for mana steal.
stealMana :: Mana -> Source -> Maybe Source
stealMana m s =
  do Source { .. } <- takeMana m s
     guard (sourceSize > 0)
     return Source { sourceSize = sourceSize - 1, .. }

-- | Increases the number of dice in the source by one.
returnStolenMana :: Source -> Source
returnStolenMana Source { .. } = Source { sourceSize = 1 + sourceSize, .. }

-- | Set a unit of the first mana to the second.  The new mana is not
-- available until the next turn.  See "Mana Draw".
advancedManaDraw :: Mana -> Mana -> Source -> Maybe Source
advancedManaDraw mFrom mTo s =
  do guard (mTo /= Gold)
     Source { .. } <- takeMana mFrom s
     return Source { sourceFixed = bagAdd 1 mTo sourceFixed, .. }


--------------------------------------------------------------------------------
instance Export Source where
  toJS Source { .. } = toJS (bagToList sourceMana)


