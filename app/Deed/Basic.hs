module Deed.Basic (allBasic) where

import Data.Set qualified as Set

import Common.Field
import Common.Interact

import Common
import State
import Input
import Source
import ManaPool

import AppTypes
import Deed.Action


allBasic :: Deeds
allBasic = unionDeeds
  [ white
  ]


white :: Deeds
white = defDeeds
  [ defDeed "Mana Pull" basicManaPull powerManaPull
  ]

basicManaPull :: DeedDef
basicManaPull s =
  do mb <- chooseMaybe pid "Extra die" [ (Source m, "Mana Pull") | m <- avail ]
     case mb of
       Just (Source m) ->
         do mv <- case m of
                    Black ->
                      inpMana <$>
                      choose pid "Mana Pull (black)"
                         [ (AskMana tm, "Use as") | tm <- anyMana ]
                    _ -> pure m
            update
              $ SetState
              $ setField source (takeMana m sourceVal)
              $ setField mana   (addSourceMana mv manaVal)
                s
       _ -> pure ()

  where
  pid       = playerId s
  sourceVal = getField source s
  manaVal   = getField mana s
  avail     = availableMana sourceVal


powerManaPull :: DeedDef
powerManaPull _ = pure ()
{-
  where
  count done
    | done >= 2 = pure ()
    | otherwise =
      do 
-}



