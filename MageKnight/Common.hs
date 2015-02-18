{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.Common where

import MageKnight.Bag
import MageKnight.JSON

import           Data.Text ( Text )
import qualified Data.Text as Text
import           Text.PrettyPrint
import           Data.Char(toLower)


type PlayerId   = Int


data Visibility = Revealed | Hidden
                  deriving (Eq,Ord,Show)

data Element    = Physycal | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show)

data Terrain    = Plains | Hills | Forest | Wasteland | Desert | Swamp
                | City BasicMana
                | Lake | Mountain
                | Ocean {- for tile A and B -}
                  deriving (Eq,Ord,Show)

data TerrainCostChange =
    DecreaseTo Int      -- ^ Decrease to the given value
  | DecreaseBy Int Int  -- ^ Decrease by amount, to minimum
    deriving (Eq,Ord,Show)


type DeedName = Text
type EnemyName = Text



data Resource =

    ManaToken Mana
  | ManaCrystal BasicMana

  | ManaSource Mana
  | ManaDie

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element

  | Healing
  | ReadyUnit Int   -- ^ Ready a unit if this level

  | ADeed DeedName

  | Blocking EnemyName

  | ChangeTerrainCost Terrain TerrainCostChange

  | DrawDeed

  -- End of turn
  | ReputationLoss
  | RegainUsedCrystals
  | ManaSourceFixed Mana    -- Not re-rolled
  | FameGainIfInteract        -- ^ Noble Manners
  | ReputationGainIfInteract  -- ^ Noble Manners
  | ThrowAway DeedName
  | ToDeedDeckBottom DeedName
  | ToDeedDeckTop DeedName
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
    ADeed x  -> text (show x) <+> text "card"

    -- XXX
    ChangeTerrainCost t c -> text "change terrain cost"

    Blocking x -> text "blocking" <+> text (Text.unpack x)

    ReputationLoss -> text "reputation -1"
    DrawDeed -> text "draw a card"
    FameGainIfInteract -> text "fame +1 (if interacted)"
    ReputationGainIfInteract -> text "reputation + 1 (if interacted)"
    ThrowAway x -> text "throw away" <+> text (show x)
    ToDeedDeckBottom x -> text "place" <+> text (show x) <+>
                          text "at the bottom of the deed deck"

    ToDeedDeckTop x -> text "place" <+> text (show x) <+>
                       text "at the top of the deed deck"



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

instance ExportAsKey Time where
  toKeyJS t = case t of
                Day   -> "day"
                Night -> "night"

instance Export Time where
  toJS = jsKey

instance ExportAsKey BasicMana where
  toKeyJS m = case m of
                White -> "white"
                Red   -> "red"
                Blue  -> "blue"
                Green -> "green"

instance Export BasicMana where
  toJS = jsKey

instance ExportAsKey Mana where
  toKeyJS m = case m of
                Gold        -> "gold"
                Black       -> "black"
                BasicMana b -> toKeyJS b

instance Export Mana where
  toJS = jsKey


instance ExportAsKey Element where
  toKeyJS m = case m of
                Physycal    -> "physical"
                Fire        -> "fire"
                Ice         -> "ice"
                ColdFire    -> "cold_fire"

instance Export Element where
  toJS = jsKey

instance ExportAsKey AttackType where
  toKeyJS m = case m of
                Melee   -> "melee"
                Ranged  -> "ranged"
                Siege   -> "siege"

instance Export AttackType where
  toJS = jsKey

instance ExportAsKey Terrain where
  toKeyJS t =
    case t of
      Plains    -> "plains"
      Hills     -> "hills"
      Forest    -> "forest"
      Wasteland -> "wasteland"
      Desert    -> "desert"
      Swamp     -> "swamp"
      City m    -> Text.append "city_" (toKeyJS m)
      Lake      -> "lake"
      Mountain  -> "mountain"
      Ocean     -> "ocean"


