module AppTypes where

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON,FromJSON)

import Source

data State = State PlayerId Source
  deriving (Generic,ToJSON)

finalState :: State -> Bool
finalState = const False

data Input = InputA () | InputB
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


