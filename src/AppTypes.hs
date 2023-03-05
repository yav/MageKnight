module AppTypes where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

import Common.Basics(PlayerId)
import Common.Field(declareFields)

import Common
import Source
import Enemies

data State = State
  { _playerId :: PlayerId
  , _source   :: Source
  , _enemies  :: [Enemy]
  }
  deriving (Generic,ToJSON)

declareFields ''State

finalState :: State -> Bool
finalState = const False

data Input = Source Mana
           | AskMana Mana
           | TestReroll
           | TestFixed
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

--------------------------------------------------------------------------------
-- No interesting updates or multiple players

data Update = SetState State

doUpdate   :: Update -> State -> State
doUpdate (SetState s) _ = s

type StateView = State

playerView :: PlayerId -> State -> StateView
playerView _ = id


type UpdateView = StateView

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView p (SetState s) = playerView p s


