module Input where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Aeson(ToJSON,FromJSON)

import Common
import Terrain

data Input = Source Mana      -- ^ Mana in the source
           | AskMana Mana     -- ^ Just mana color
           | AskManaPool Mana -- ^ Mana in the mana pool

           -- Hand management
           | AskHand Int
           | AskSelectedSideways
           | AskSelectedAdvanced

           | AskLoc Addr (Maybe TileType)
             -- ^ If Just ty, then tile is off board
             -- and we want to explore it

           | ActionButton Text

           | TestReroll
           | TestFixed
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

inpMana :: Input -> Mana
inpMana inp =
  case inp of
    Source m -> m
    AskMana m -> m
    AskManaPool m -> m
    _ -> error "inpMana: not implemented"
