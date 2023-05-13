module Game.KOI (MK(..), Update(..), Interact, module KOI.Interact) where

import KOI.Interact hiding (Interact)
import KOI.Interact qualified as I

import Game.State
import Game.Input

data MK = MK

type Interact = I.Interact MK

instance Component MK where
  type AppState MK = State
  type AppStateView MK = State
  type AppUpdate MK = Update
  type AppUpdateView MK = State
  type AppInput MK = Input

  doUpdate _ (SetState s) _ = s
  playerView _ _ = id
  playerUpdateView c p (SetState s) = playerView c p s
  finalState _ _ = False

data Update = SetState State
