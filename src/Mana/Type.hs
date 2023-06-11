module Mana.Type where

import Data.Char(toLower)

import KOI.Enum
import KOI.PP

data BasicMana  = Red | Green | Blue | White
                  deriving (Eq,Ord,Show,Read)

data Mana       = BasicMana BasicMana | Gold | Black
                  deriving (Eq,Ord,Show,Read)

anyBasicMana :: [ BasicMana ]
anyBasicMana = [ Red, Green, Blue, White ]

anyMana :: [ Mana ]
anyMana = Gold : Black : map BasicMana anyBasicMana

instance PP BasicMana where
  pp = pp . map toLower . show

instance PP Mana where
  pp m =
    case m of
      BasicMana b -> pp b
      Gold        -> "gold"
      Black       -> "black"

--------------------------------------------------------------------------------

declareEnumText ''BasicMana
declareEnumText ''Mana


