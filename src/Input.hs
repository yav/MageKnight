module Input where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Aeson(ToJSON,FromJSON)

import Common

data Input = Source { inpMana :: Mana }      -- ^ Mana in the source
           | AskMana { inpMana :: Mana }     -- ^ Just mana color
           | AskManaPool { inpMana :: Mana } -- ^ Mana in the mana pool

           -- Hand management
           | AskHand Int
           | AskSelectedSideways
           | AskSelectedAdvanced

           | ActionButton Text

           | TestReroll
           | TestFixed
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

