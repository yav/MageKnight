module AppTypes
  ( module AppTypes
  , module State
  , module Input
  ) where

import Common.Basics(PlayerId)

import State
import Input


--------------------------------------------------------------------------------
-- No interesting updates or multiple players

-- This is a data because jsHandlers only supports data
data Update = SetState State

doUpdate   :: Update -> State -> State
doUpdate (SetState s) _ = s

type StateView = State

playerView :: PlayerId -> State -> StateView
playerView _ = id


type UpdateView = StateView

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView p (SetState s) = playerView p s


