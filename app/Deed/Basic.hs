module Deed.Basic (allBasic) where

import Data.Text qualified as Text

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
powerManaPull = count 1
  where
  count :: Int -> State -> Interact ()
  count dieNum s
    | dieNum > 2 = pure ()
    | otherwise =
      do mb <- chooseMaybe pid msg [ (Source m, "Set and use") | m <- avail ]
         case mb of
           Nothing -> count 3 s
           Just mi ->
             do let m = inpMana mi
                tgt <- inpMana <$>
                       choose pid "Set to"
                          [ (AskMana t,"Set to") | t <- anyMana, t /= Gold ]
                update
                  $ SetState
                  $ setField source (takeAndConvertMana m tgt sourceVal)
                  $ setField mana   (addMana tgt manaVal) s
                count (dieNum + 1) =<< getState
      where
      pid       = playerId s
      sourceVal = getField source s
      manaVal   = getField mana s
      avail     = availableMana sourceVal
      msg       = "Mana Pull (" <> Text.pack (show dieNum) <> "/2)"




