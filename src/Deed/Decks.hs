module Deed.Decks (playDeed) where

import Hand(SelectedMode(..))

import Deed.Type
import Deed.Action
import Deed.Basic(allBasic)


playDeed :: SelectedMode -> DeedName -> Interact()
playDeed mode ca =
  case mode of
    SelectedSideways -> notImplemented "(sideways)"
    SelectedBasic    -> actBasic (getDeedAction ca)
    SelectedAdvanced -> actPower (getDeedAction ca)

getDeedAction :: DeedName -> DeedAction
getDeedAction nm = getAction nm allDeedActions

allDeedActions :: Deeds
allDeedActions =
  unionDeeds
    [ allBasic
    ]



