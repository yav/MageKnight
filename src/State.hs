module State where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Basics(PlayerId)
import Common.Field(declareFields)
import Common.Bag

import Common
import Hand
import Source

data State = State
  { playerId  :: PlayerId
  , _source   :: Source
  , _hand     :: Hand
  , _mana     :: Bag Mana
  }
  deriving (Generic,ToJSON)

declareFields ''State

finalState :: State -> Bool
finalState = const False


