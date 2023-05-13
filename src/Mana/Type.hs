module Mana.Type where

import Text.PrettyPrint
import Data.Char(toLower)

import KOI.Enum

data BasicMana  = Red | Green | Blue | White
                  deriving (Eq,Ord,Show,Read)

data Mana       = BasicMana BasicMana | Gold | Black
                  deriving (Eq,Ord,Show,Read)

anyBasicMana :: [ BasicMana ]
anyBasicMana = [ Red, Green, Blue, White ]

anyMana :: [ Mana ]
anyMana = Gold : Black : map BasicMana anyBasicMana

ppBasicMana :: BasicMana -> Doc
ppBasicMana = text . map toLower . show

ppMana :: Mana -> Doc
ppMana m =
  case m of
    BasicMana b -> ppBasicMana b
    Gold        -> text "gold"
    Black       -> text "black"

--------------------------------------------------------------------------------

declareEnumText ''BasicMana
declareEnumText ''Mana


