module Input where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

import Common

data Input = Source Mana            -- ^ Mana in the source
           | AskMana Mana           -- ^ Questoin about mana in general
           | AskManaPool Mana       -- ^ Mana in the mana pool

           -- Hand management
           | AskHand Int
           | AskSelectedSideways
           | AskSelectedAdvanced


           | TestReroll
           | TestFixed
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

