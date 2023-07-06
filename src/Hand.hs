module Hand where

import GHC.Generics
import Data.Aeson
import Optics

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

makeLenses ''Hand
makeLenses ''SelectedDeed


newHand :: [Deed] -> Hand
newHand ds = Hand { _handCards = ds, _handSelected = Nothing }

handSelect :: Int -> Hand -> Hand
handSelect i h =
  case splitAt i (view handCards h) of
    (as, d : bs) ->
      set handSelected (Just (newSelected d)) $
      set handCards (as ++ bs) h
    _ -> h

handSelectMode :: SelectedMode -> Hand -> Hand
handSelectMode m h =
  case view handSelected h of
    Nothing -> h
    Just s -> set handSelected (Just $ set selectedMode m s) h

handPlayable :: Hand -> [Int]
handPlayable =
    map fst
  . filter ((/= Wound) . snd)
  . zip [0..]
  . view handCards

newSelected :: Deed -> SelectedDeed
newSelected d = SelectedDeed
  { _selectedDeed = d
  , _selectedMode = SelectedBasic
  }


