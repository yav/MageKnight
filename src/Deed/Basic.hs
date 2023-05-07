module Deed.Basic (allBasic) where

import Deed.Action
import Deed.ManaPull


allBasic :: Deeds
allBasic = unionDeeds
  [ white
  ]


white :: Deeds
white = defDeeds
  [ defDeed "Mana Pull" basicManaPull powerManaPull
  , defDeed "Stamina" (const (gainMove 2)) (const (gainMove 4))
  , defDeed "March"   (const (gainMove 2)) (const (gainMove 4))
  ]


