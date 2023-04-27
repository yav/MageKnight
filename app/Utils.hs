module Utils where

import Data.Text(Text)
import KOI.Basics

import AppTypes
import Input

sync :: Interact ()
sync =
  do s <- getState
     update (SetState s)


type InputOption a = (WithPlayer Input, Text, Interact a)
