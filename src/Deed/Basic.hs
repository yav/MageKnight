module Deed.Basic (allBasic) where

import Deed.Type
import Deed.Action
import Deed.ManaPull


allBasic :: Deeds
allBasic = unionDeeds
  [ white
  ]


white :: Deeds
white = defDeeds
  [ defDeed (ActionName Mana_Pull) basicManaPull powerManaPull
  , defDeed (ActionName Stamina) (const (gainMove 2)) (const (gainMove 4))
  , defDeed (ActionName March)   (const (gainMove 2)) (const (gainMove 4))
  ]


