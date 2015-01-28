{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.Common where

import MageKnight.Bag
import MageKnight.JSON

import Data.Text ( Text )
import Text.PrettyPrint
import Data.Char(toLower)


type PlayerId   = Int


data Visibility = Revealed | Hidden
                  deriving (Eq,Ord,Show)

data Element    = Physycal | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show)



type CardName = Text

data Resource =

    ManaToken Mana
  | ManaCrystal BasicMana
  | SpentManaCrystal BasicMana

  | ManaSource Mana
  | ManaDie

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element

  | Healing

  | ACard CardName


  | DrawCard

  -- End of turn
  | ReputationLoss
  | RegainUsedCrystals
  | ManaSourceFixed Mana    -- Not re-rolled

    deriving (Eq,Ord,Show)

data AttackType = Melee | Ranged | Siege
                  deriving (Eq,Ord,Show)


data BasicMana = Red | Green | Blue | White
                 deriving (Eq,Ord,Show)

data Mana = BasicMana BasicMana | Gold | Black
            deriving (Eq,Ord,Show)

data Time = Day | Night
            deriving (Eq,Ord,Show)



anyBasicMana :: [ BasicMana ]
anyBasicMana = [ Red, Green, Blue, White ]

anyMana :: [ Mana ]
anyMana = Gold : Black : map BasicMana anyBasicMana

anyAttack :: [ AttackType ]
anyAttack = [ Melee, Ranged, Siege ]


-- Pretty Print

ppResource :: Resource -> Doc
ppResource resource =
  case resource of
    ManaToken m -> ppMana m <+> text "token"
    ManaCrystal m -> ppBasicMana m <+> text "crystal"
    SpentManaCrystal m -> ppBasicMana m <+> text "crystal (spent)"
    RegainUsedCrystals -> text "regain unused crystals"
    ManaSource m -> ppMana m <+> text "source"
    ManaSourceFixed m -> ppMana m <+> text "source (fixed)"
    ManaDie -> text "mana die"

    Movement -> text "movement"
    Influence -> text "influence"
    Attack at el -> elDoc <+> tyDoc <+> text "attack"
      where elDoc = ppElement el
            tyDoc = case at of
                      Melee  -> empty
                      Ranged -> text "ranged"
                      Siege  -> text "siege"
    Block e  -> ppElement e
    Healing  -> text "healing"
    ACard x  -> text (show x) <+> text "card"

    ReputationLoss -> text "reputation -1"
    DrawCard -> text "draw a card"

ppElement :: Element -> Doc
ppElement el =
  case el of
    Physycal  -> text "physycal"
    Fire      -> text "fire"
    Ice       -> text "ice"
    ColdFire  -> text "cold fire"

ppBasicMana :: BasicMana -> Doc
ppBasicMana = text . map toLower . show

ppMana :: Mana -> Doc
ppMana m =
  case m of
    BasicMana b -> ppBasicMana b
    Gold        -> text "gold"
    Black       -> text "black"

ppResources :: Bag Resource -> Doc
ppResources = vcat . map ppEntry . bagToListGrouped
  where ppEntry (r,x) = int x <+> ppResource r



--------------------------------------------------------------------------------

instance Export Time where
  toJS t = toJS (txt :: Text)
    where
    txt = case t of
            Day   -> "day"
            Night -> "night"

instance Export BasicMana where
  toJS m = toJS (txt :: Text)
    where
    txt = case m of
            White -> "white"
            Red   -> "red"
            Blue  -> "blue"
            Green -> "green"

instance Export Mana where
  toJS m = case m of
             Gold        -> toJS ("gold" :: Text)
             Black       -> toJS ("black" :: Text)
             BasicMana b -> toJS b



