module Utils where

import Data.Text(Text)
import KOI.Basics

import Game.KOI
import Game.Input

type InputOption a = (WithPlayer Input, Text, Interact a)

sync :: Interact ()
sync =
  do s <- getState
     update (SetState s)


