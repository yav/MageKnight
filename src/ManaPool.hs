module ManaPool
  ( ManaPool
  , emptyManaPool
  , addSourceMana, addMana
  , hasMana
  , removeMana
  , removeManaMaybe
  ) where

import GHC.Generics
import Control.Applicative((<|>))
import Data.Maybe(fromMaybe)
import Data.Aeson
import Common.Bag

import Common


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

