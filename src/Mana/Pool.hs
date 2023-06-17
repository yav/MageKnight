module Mana.Pool
  ( ManaPool
  , emptyManaPool

    -- * Normal Mana
  , addSourceMana, addMana
  , hasMana
  , removeMana
  , removeManaMaybe
  , convertMana

    -- * Crystals
  , addCrystal
  , useCrystal
  , hasCrystal
  ) where

import GHC.Generics
import Control.Applicative((<|>))
import Data.Maybe(fromMaybe)
import Data.Aeson

import KOI.Bag

import Mana.Type


data ManaPool = ManaPool
  { mpMana      :: Bag Mana       -- ^ May be discarded
  , mpSource    :: Bag Mana       -- ^ Has to be used before end of round
  , mpCrystals  :: Bag BasicMana  -- ^ Persists across rounds
  } deriving (Generic, ToJSON)

emptyManaPool :: ManaPool
emptyManaPool = ManaPool
  { mpMana      = bagEmpty
  , mpSource    = bagEmpty
  , mpCrystals  = bagEmpty
  }

addSourceMana :: Mana -> ManaPool -> ManaPool
addSourceMana m mp = mp { mpSource = bagChange 1 m (mpSource mp) }

addMana :: Mana -> ManaPool -> ManaPool
addMana m mp = mp { mpMana = bagChange 1 m (mpMana mp) }

addCrystal :: BasicMana -> ManaPool -> ManaPool
addCrystal m mp = mp { mpCrystals = bagChange 1 m (mpCrystals mp) }

useCrystal :: BasicMana -> ManaPool -> ManaPool
useCrystal m mp =
  case bagChangeMaybe (-1) m (mpCrystals mp) of
    Just newCrystals -> addMana (BasicMana m) mp { mpCrystals = newCrystals }
    Nothing -> mp

hasCrystal :: BasicMana -> ManaPool -> Bool
hasCrystal m mp = bagContains m (mpCrystals mp) > 0

convertMana :: Mana -> Mana -> ManaPool -> ManaPool
convertMana fromM toM mp =
  fromMaybe mp $
    do yes <- attempt (mpSource mp)
       pure mp { mpSource = yes }
    <|>
    do yes <- attempt (mpMana mp)
       pure mp { mpMana = yes }
  where
  attempt b = bagChange 1 toM <$> bagChangeMaybe (-1) fromM b

hasMana :: Int -> Mana -> ManaPool -> Bool
hasMana amt m mp = amt <= tot
  where
  tot = bagContains m (mpSource mp) + bagContains m (mpMana mp)

removeManaMaybe :: Mana -> ManaPool -> Maybe ManaPool
removeManaMaybe m mp =
  do b <- bagChangeMaybe (-1) m (mpSource mp)
     pure mp { mpSource = b }
  <|>
  do b <- bagChangeMaybe (-1) m (mpMana mp)
     pure mp { mpMana = b }

removeMana :: Mana -> ManaPool -> ManaPool
removeMana m mp = fromMaybe mp (removeManaMaybe m mp)

