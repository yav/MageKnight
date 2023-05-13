module Mana.Pool
  ( ManaPool
  , emptyManaPool
  , addSourceMana, addMana
  , hasMana
  , removeMana
  , removeManaMaybe
  , convertMana
  ) where

import GHC.Generics
import Control.Applicative((<|>))
import Data.Maybe(fromMaybe)
import Data.Aeson

import KOI.Bag

import Mana.Type


data ManaPool = ManaPool
  { mpMana   :: Bag Mana    -- ^ May be discarded
  , mpSource :: Bag Mana    -- ^ Has toi be used before end of round
  } deriving (Generic, ToJSON)

emptyManaPool :: ManaPool
emptyManaPool = ManaPool
  { mpMana = bagEmpty
  , mpSource = bagEmpty
  }

addSourceMana :: Mana -> ManaPool -> ManaPool
addSourceMana m mp = mp { mpSource = bagChange 1 m (mpSource mp) }

addMana :: Mana -> ManaPool -> ManaPool
addMana m mp = mp { mpMana = bagChange 1 m (mpMana mp) }

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

