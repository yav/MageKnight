module Deed.Decks (playDeed) where

import Data.Text(Text)
import KOI.Interact hiding (Interact)

import Hand(SelectedMode(..))

import Deed.Action
import Deed.Basic(allBasic)
import AppTypes


playDeed :: SelectedMode -> Text -> Interact()
playDeed mode ca =
  case mode of
    SelectedSideways -> notImplemented "(sideways)"
    SelectedBasic    -> actBasic (getDeedAction ca)
    SelectedAdvanced -> actPower (getDeedAction ca)

getDeedAction :: Text -> DeedAction
getDeedAction nm = getAction nm allDeedActions

allDeedActions :: Deeds
allDeedActions =
  unionDeeds
    [ allBasic
    ]
