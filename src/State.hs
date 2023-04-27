module State where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import KOI.Basics(PlayerId)
import KOI.Field(declareFields)

import Hand
import Source
import ManaPool
import Land
import Hero

data State = State
  { playerId      :: PlayerId
  , playerHero    :: Hero
  , _source       :: Source
  , _sourceUsed   :: Bool
  , _hand         :: Hand
  , _mana         :: ManaPool
  , _land         :: Land
  }
  deriving (Generic,ToJSON)

declareFields ''State

finalState :: State -> Bool
finalState = const False


