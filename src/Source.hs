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

import GHC.Generics
import Data.Set(Set)
import Control.Monad (replicateM)

import Common.Bag
import Common.RNGM
import Data.Aeson(ToJSON)

import Common

-- | The source of pure mana
data Source = Source
  { sourceMana    :: Bag Mana     -- Available
  , sourceFixed   :: Bag Mana     -- Unavalable, not re-reroled
  , sourceUsed    :: Bag Mana     -- Unavalable, reroll
  , sourceRNG     :: RNG
  } deriving (Generic,ToJSON,Show)

sourceSize :: Source -> Int
sourceSize Source { .. } =
  bagSize sourceMana + bagSize sourceFixed + bagSize sourceUsed


-- | Mana present in the source.
availableMana :: Source -> Set Mana
availableMana = bagToSet . sourceMana

-- | Roll this many mana dice.
rollDice :: Int -> Gen [Mana]
rollDice n = replicateM n (pickOne anyMana)

-- | Make a new source, with the given number of dice.
newSource :: Int -> Gen Source
newSource size =
  do sourceMana <- mkSource
     sourceRNG  <- splitRNG
     return Source { sourceFixed = bagEmpty, sourceUsed = bagEmpty, .. }
  where
  mkSource = do ds <- rollDice size
                let b = bagFromList ds
                if validBag b then return b else mkSource

  validBag x = (bagContains Gold x + bagContains Black x) <= div size 2

-- | Replenish spent mana.  Used at end of turn.
refillSource :: Source -> Source
refillSource Source { .. } =
  withRNG sourceRNG $
    do ds <- rollDice (bagSize sourceUsed)
       pure \rng ->
         Source { sourceRNG   = rng
                , sourceMana  = bagUnion (bagFromList ds) currentDice
                , sourceFixed = bagEmpty
                , sourceUsed  = bagEmpty
                , ..
                }
  where
  currentDice = bagUnion sourceFixed sourceMana
  -- unused, or used but fixed

-- | Setup a whole new source.  Used at the end of a round.
-- Assumes that stolen mana has been returned.
renewSource :: Source -> Source
renewSource src =
  withRNG (sourceRNG src)
    do s1 <- newSource (sourceSize src)
       pure \_ -> s1



-- | Try to take some mana from the source.
takeMana :: Mana -> Source -> Maybe Source
takeMana m Source { .. } =
  do b <- bagChangeMaybe (-1) m sourceMana
     return Source { sourceMana = b, sourceUsed = bagChange 1 m sourceUsed, .. }

-- | Take some mana from the source, and reduce its size.
-- Used for mana steal.
stealMana :: Mana -> Source -> Maybe Source
stealMana m s =
  do Source { .. } <- takeMana m s
     return Source { sourceUsed = bagChange (-1) m sourceUsed, .. }

-- | Increases the number of dice in the source by one.
returnStolenMana :: Mana -> Source -> Source
returnStolenMana m Source { .. } =
  Source { sourceUsed = bagChange 1 m sourceUsed, .. }

-- | Set a unit of the first mana to the second.  The new mana is not
-- available until the next turn.
takeAndConvertMana :: Mana -> Mana -> Source -> Maybe Source
takeAndConvertMana mFrom mTo s =
  do Source { .. } <- takeMana mFrom s
     return Source { sourceFixed = bagChange 1 mTo sourceFixed, .. }


