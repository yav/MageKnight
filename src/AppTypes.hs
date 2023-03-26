module AppTypes
  ( module AppTypes
  , module State
  ) where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

import Common.Basics(PlayerId)

import Common
import State

data Input = Source Mana
           | AskMana Mana

           -- Hand management
           | AskHand Int
           | AskSelectedSideways
           | AskSelectedAdvanced


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


