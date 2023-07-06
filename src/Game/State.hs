module Game.State where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)
import Optics

import KOI.Basics(PlayerId)

import Hand
import Mana.Source
import Mana.Pool
import Terrain.Map
import Hero
import Combat

data Phase =
    PreMovePhase
  | MovePhase
  | ActionPhase ActionPhase
    deriving (Generic,ToJSON)

data ActionPhase =
    CombatAction Combat
  | InteractAction
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
  , _heal         :: !Int
  , _phase        :: Phase
  }
  deriving (Generic,ToJSON)

makeLenses ''State



