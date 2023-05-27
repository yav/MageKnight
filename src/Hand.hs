module Hand where

import GHC.Generics
import Data.Aeson

import KOI.Field

import Deed.Type

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
    deriving (Eq, Generic, ToJSON)

declareFields ''Hand
declareFields ''SelectedDeed

newHand :: [Deed] -> Hand
newHand ds = Hand { _handCards = ds, _handSelected = Nothing }

handSelect :: Int -> Hand -> Hand
handSelect i h =
  case splitAt i (getField handCards h) of
    (as, d : bs) ->
      setField handSelected (Just (newSelected d)) $
      setField handCards (as ++ bs) h
    _ -> h

handSelectMode :: SelectedMode -> Hand -> Hand
handSelectMode m h =
  case getField handSelected h of
    Nothing -> h
    Just s -> setField handSelected (Just $ setField selectedMode m s) h

handPlayable :: Hand -> [Int]
handPlayable =
    map fst
  . filter ((/= Wound) . snd)
  . zip [0..]
  . getField handCards

newSelected :: Deed -> SelectedDeed
newSelected d = SelectedDeed
  { _selectedDeed = d
  , _selectedMode = SelectedBasic
  }


