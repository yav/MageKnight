module Input where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

import Common

data Input = Source Mana
           | AskMana Mana

           -- Hand management
           | AskHand Int
           | AskSelectedSideways
           | AskSelectedAdvanced


           | TestReroll
           | TestFixed
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

