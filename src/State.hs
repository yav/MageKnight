module State where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import KOI.Basics(PlayerId)
import KOI.Field(declareFields)

import Hand
import Source
import ManaPool
import Terrain.Map
import Hero

data Phase =
    PreMovePhase
  | MovePhase
  | ActionPhase ActionPhase
    deriving (Generic,ToJSON)

data ActionPhase = ActionPhaseXXX
    deriving (Generic,ToJSON)

data State = State
  { playerId      :: PlayerId
  , playerHero    :: Hero
  , _source       :: Source
  , _sourceUsed   :: Bool
  , _hand         :: Hand
  , _mana         :: ManaPool
  , _land         :: Land
  , _movement     :: !Int
  , _phase        :: Phase
  }
  deriving (Generic,ToJSON)

declareFields ''State



