module State where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Basics(PlayerId)
import Common.Field(declareFields)

import Hand
import Source
import ManaPool

data State = State
  { playerId    :: PlayerId
  , _source     :: Source
  , _sourceUsed :: Bool
  , _hand       :: Hand
  , _mana       :: ManaPool
  }
  deriving (Generic,ToJSON)

declareFields ''State

finalState :: State -> Bool
finalState = const False


