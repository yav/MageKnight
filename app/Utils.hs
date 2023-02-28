module Utils where

import Common.Interact

import AppTypes

sync :: Interact ()
sync =
  do s <- getState
     update (SetState s)
