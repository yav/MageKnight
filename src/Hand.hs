module Hand where

import GHC.Generics
import Data.Aeson

import Common.Field

import Deed

data Hand = Hand
  { _handCards    :: [Deed]
  , _handSelected :: Maybe SelectedDeed
  } deriving (Generic,ToJSON)

data SelectedDeed = SelectedDeed
  { _selectedDeed :: Deed
  , _selectedMode :: SelectedMode
  } deriving (Generic,ToJSON)

data SelectedMode =
    SelectedBasic
  | SelectedAdvanced
  | SelectedSideways
    deriving (Generic, ToJSON)

declareFields ''Hand
declareFields ''SelectedDeed

newHand :: [Deed] -> Hand
newHand ds = Hand { _handCards = ds, _handSelected = Nothing }

handSelect :: Int -> Hand -> Hand
handSelect i h =
  case drop i (getField handCards h) of
    d : _ -> setField handSelected (Just (newSelected d)) h
    _ -> h

handSelectMode :: SelectedMode -> Hand -> Hand
handSelectMode m h =
  case getField handSelected h of
    Nothing -> h
    Just s -> setField handSelected (Just $ setField selectedMode m s) h

newSelected :: Deed -> SelectedDeed
newSelected d = SelectedDeed
  { _selectedDeed = d
  , _selectedMode = SelectedBasic
  }


