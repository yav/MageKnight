module Utils where

import Data.Text(Text)
import Common.Basics
import Common.Interact

import Input
import AppTypes

sync =
  do s <- getState
     update (SetState s)


type InputOption a = (WithPlayer Input, Text, Interact a)
type TopInputOption = InputOption ()
