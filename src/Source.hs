{-# LANGUAGE RecordWildCards #-}
module Source
  ( Source
  , newSource
  , refillSource
  , renewSource

  , availableMana
  , takeMana
  , stealMana
  , returnStolenMana
  , takeAndConvertMana
  ) where

import Data.Set(Set)

import Common
import Util.Bag
import Util.Random
import Util.JSON

import Control.Monad (guard, replicateM)

-- | The source of pure mana
data Source = Source
  { sourceMana    :: Bag Mana     -- Available
  , sourceFixed   :: Bag Mana     -- Unavalable, not re-reroled
  , sourceRNG     :: StdGen
  , sourceSize    :: Int          -- Size when full
  }

-- | Mana present in the source.
availableMana :: Source -> Set Mana
availableMana = bagToSet . sourceMana

-- | Roll this many mana dice.
rollDice :: Int -> Gen [Mana]
rollDice n = replicateM n (oneOf anyMana)

-- | Make a new source, with the given number of dice.
newSource :: Int -> Gen Source
newSource sourceSize =
  do sourceMana <- mkSource
     sourceRNG  <- randStdGen
     return Source { sourceFixed = bagEmpty, .. }
  where
  mkSource = do ds <- rollDice sourceSize
                let b = bagFromList ds
                if validBag b then return b else mkSource

  validBag x = (bagLookup Gold x + bagLookup Black x) <= div sourceSize 2

-- | Replenish spent mana.  Used at end of turn.
refillSource :: Source -> Source
refillSource Source { .. } =
  genRandFun sourceRNG $
    do ds <- rollDice (sourceSize - bagSize currentDice)
       return $ \rng ->
         Source { sourceRNG   = rng
                , sourceMana  = bagUnion (bagFromList ds) currentDice
                , sourceFixed = bagEmpty
                , ..
                }
  where
  currentDice = bagUnion sourceFixed sourceMana
  -- unused, or used but fixed

-- | Setup a whole new source.  Used at the end of a round.
-- Assumes that stolen mana has been returned.
renewSource :: Source -> Source
renewSource Source { .. } =
  genRandFun sourceRNG $
     do s1 <- newSource sourceSize
        return $ \_ -> s1



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
-- available until the next turn.
takeAndConvertMana :: Mana -> Mana -> Source -> Maybe Source
takeAndConvertMana mFrom mTo s =
  do Source { .. } <- takeMana mFrom s
     return Source { sourceFixed = bagAdd 1 mTo sourceFixed, .. }


--------------------------------------------------------------------------------
-- XXX: Should export fixed mana also
instance Export Source where
  toJS Source { .. } = toJS (bagToList sourceMana)


